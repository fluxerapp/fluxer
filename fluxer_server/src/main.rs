// SPDX-License-Identifier: AGPL-3.0-or-later

mod env;

use anyhow::{Context as _, anyhow, bail};
use env::Source;
use fluxer_common::config::EnvMap;
use std::any::Any;
use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::process::ExitCode;
use std::sync::Arc;
use std::time::Duration;
use tokio::task::JoinSet;
use tokio::time::Instant;
use tokio_util::sync::CancellationToken;
use tracing::Instrument as _;

type Task = Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send>>;

#[derive(Clone, Copy)]
struct Service {
    name: &'static str,
    start: fn(EnvMap, CancellationToken) -> Task,
}

macro_rules! services {
    ($($name:literal => $krate:ident),* $(,)?) => {
        [$(Service {
            name: $name,
            start: |env, shutdown| {
                Box::pin(async move { $krate::run($krate::Config::from_env(&env)?, shutdown).await })
            },
        }),*]
    };
}

fn catalogue() -> [Service; 8] {
    services!(
        "snowflakes" => fluxer_snowflakes,
        "users" => fluxer_users,
        "messages" => fluxer_messages,
        "gifs" => fluxer_gifs,
        "unfurl" => fluxer_unfurl,
        "push" => fluxer_push,
        "app-proxy" => fluxer_app_proxy,
        "admin" => fluxer_admin,
    )
}

const GATING: [&str; 4] = ["snowflakes", "users", "messages", "gifs"];

#[derive(Clone, Copy)]
struct Backoff {
    min: Duration,
    max: Duration,
    healthy_after: Duration,
}

const BACKOFF: Backoff = Backoff {
    min: Duration::from_secs(1),
    max: Duration::from_secs(30),
    healthy_after: Duration::from_secs(60),
};

fn main() -> ExitCode {
    match start() {
        Ok(code) => code,
        Err(error) => {
            eprintln!("fluxer-server: {error:#}");
            ExitCode::FAILURE
        }
    }
}

fn start() -> anyhow::Result<ExitCode> {
    let mut args = std::env::args().skip(1);
    let healthcheck = match args.next().as_deref() {
        None => false,
        Some("healthcheck") => true,
        Some(other) => bail!("unknown command {other}, the only command is healthcheck"),
    };
    if let Some(extra) = args.next() {
        bail!("unexpected argument {extra}");
    }

    let catalogue = catalogue();
    let known: Vec<&'static str> = catalogue.iter().map(|service| service.name).collect();
    let source = Source::from_process(&known)?;
    let enabled = source.services(&known)?;

    if healthcheck {
        return tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?
            .block_on(check_health(&source, &enabled));
    }

    rustls::crypto::aws_lc_rs::default_provider()
        .install_default()
        .map_err(|_| anyhow!("a rustls crypto provider was installed before fluxer-server could install aws-lc-rs"))?;
    fluxer_svc::init_tracing(&source.process());

    let services: Vec<Service> = catalogue
        .into_iter()
        .filter(|service| enabled.contains(&service.name))
        .collect();
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()?;
    runtime.block_on(async {
        let shutdown = fluxer_common::shutdown::signal_token();
        run_services(services, Arc::new(source), shutdown, BACKOFF).await;
    });
    runtime.shutdown_timeout(Duration::from_secs(1));
    Ok(ExitCode::SUCCESS)
}

async fn run_services(
    services: Vec<Service>,
    source: Arc<Source>,
    shutdown: CancellationToken,
    backoff: Backoff,
) {
    let names: Vec<&str> = services.iter().map(|service| service.name).collect();
    tracing::info!(services = ?names, "starting fluxer-server");
    let mut supervisors = JoinSet::new();
    for service in services {
        supervisors.spawn(
            supervise(service, source.clone(), shutdown.clone(), backoff)
                .instrument(fluxer_common::svc_span::span(service.name)),
        );
    }
    while supervisors.join_next().await.is_some() {}
    tracing::info!("fluxer-server stopped");
}

async fn supervise(
    service: Service,
    source: Arc<Source>,
    shutdown: CancellationToken,
    backoff: Backoff,
) {
    let name = service.name;
    let mut delay = backoff.min;
    loop {
        let started = Instant::now();
        let attempt = {
            let source = source.clone();
            let token = shutdown.child_token();
            tokio::spawn(
                async move { (service.start)(source.env(name)?, token).await }.in_current_span(),
            )
        };
        let outcome = attempt.await;
        if shutdown.is_cancelled() {
            match outcome {
                Ok(Ok(())) => {}
                Ok(Err(error)) => tracing::error!("{name} stopped with an error: {error:#}"),
                Err(error) => {
                    tracing::error!("{name} panicked while stopping: {}", panic_text(error))
                }
            }
            return;
        }
        match outcome {
            Ok(Ok(())) => tracing::error!("{name} stopped on its own"),
            Ok(Err(error)) => tracing::error!("{name} failed: {error:#}"),
            Err(error) => tracing::error!("{name} panicked: {}", panic_text(error)),
        }
        if started.elapsed() >= backoff.healthy_after {
            delay = backoff.min;
        }
        tracing::warn!("restarting {name} in {} ms", delay.as_millis());
        tokio::select! {
            () = shutdown.cancelled() => return,
            () = tokio::time::sleep(delay) => {}
        }
        delay = (delay * 2).min(backoff.max);
    }
}

fn panic_text(error: tokio::task::JoinError) -> String {
    if !error.is_panic() {
        return error.to_string();
    }
    let payload: Box<dyn Any + Send> = error.into_panic();
    if let Some(text) = payload.downcast_ref::<&str>() {
        (*text).to_owned()
    } else if let Some(text) = payload.downcast_ref::<String>() {
        text.clone()
    } else {
        "unknown panic payload".to_owned()
    }
}

fn health_address(svc: &str, env: &EnvMap) -> anyhow::Result<SocketAddr> {
    use fluxer_common::healthcheck::address;
    match svc {
        "push" => address(
            env,
            "FLUXER_PUSH_SERVICE_HOST",
            "FLUXER_PUSH_SERVICE_PORT",
            fluxer_push::config::Mode::Delivery.default_port(),
        ),
        "app-proxy" => address(env, "FLUXER_APP_PROXY_HOST", "FLUXER_APP_PROXY_PORT", 8080),
        "admin" => address(env, "FLUXER_ADMIN_HOST", "FLUXER_ADMIN_PORT", 3020),
        _ => address(env, "FLUXER_SVC_LISTEN_HOST", "FLUXER_SVC_PORT", 8090),
    }
}

async fn check_health(source: &Source, enabled: &[&'static str]) -> anyhow::Result<ExitCode> {
    let mut checked: Vec<&'static str> = enabled
        .iter()
        .copied()
        .filter(|svc| GATING.contains(svc))
        .collect();
    if checked.is_empty() {
        checked = enabled.to_vec();
    }
    let mut probes = JoinSet::new();
    for svc in checked {
        let addr = health_address(svc, &source.env(svc)?)
            .with_context(|| format!("{svc} health address"))?;
        probes.spawn(async move { (svc, fluxer_common::healthcheck::run(addr).await) });
    }
    let mut failed = Vec::new();
    while let Some(result) = probes.join_next().await {
        let (svc, result) = result?;
        if let Err(error) = result {
            failed.push(format!("{svc}: {error:#}"));
        }
    }
    if failed.is_empty() {
        return Ok(ExitCode::SUCCESS);
    }
    failed.sort();
    eprintln!("fluxer-server unhealthy: {}", failed.join("; "));
    Ok(ExitCode::FAILURE)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static BAD_CONFIG: AtomicUsize = AtomicUsize::new(0);
    static PANICS: AtomicUsize = AtomicUsize::new(0);
    static STEADY: AtomicUsize = AtomicUsize::new(0);
    static STEADY_STOPPED: AtomicUsize = AtomicUsize::new(0);
    static BAD_CONFIG_AT: Mutex<Vec<Instant>> = Mutex::new(Vec::new());

    fn bad_config(_: EnvMap, _: CancellationToken) -> Task {
        BAD_CONFIG.fetch_add(1, Ordering::SeqCst);
        BAD_CONFIG_AT.lock().unwrap().push(Instant::now());
        Box::pin(async { Err(anyhow!("FLUXER_EXAMPLE is not set")) })
    }

    fn panics_once(_: EnvMap, shutdown: CancellationToken) -> Task {
        Box::pin(async move {
            if PANICS.fetch_add(1, Ordering::SeqCst) == 0 {
                panic!("first start fails");
            }
            shutdown.cancelled().await;
            Ok(())
        })
    }

    fn steady(_: EnvMap, shutdown: CancellationToken) -> Task {
        Box::pin(async move {
            STEADY.fetch_add(1, Ordering::SeqCst);
            shutdown.cancelled().await;
            STEADY_STOPPED.fetch_add(1, Ordering::SeqCst);
            Ok(())
        })
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn failing_services_retry_with_backoff_and_only_cancel_stops_the_process() {
        let services = vec![
            Service {
                name: "bad-config",
                start: bad_config,
            },
            Service {
                name: "panics-once",
                start: panics_once,
            },
            Service {
                name: "steady",
                start: steady,
            },
        ];
        let known = ["bad-config", "panics-once", "steady"];
        let source = Arc::new(Source::new(std::iter::empty(), &known).unwrap());
        let shutdown = CancellationToken::new();
        let backoff = Backoff {
            min: Duration::from_millis(20),
            max: Duration::from_millis(80),
            healthy_after: Duration::from_secs(60),
        };
        let process = tokio::spawn(run_services(services, source, shutdown.clone(), backoff));

        tokio::time::sleep(Duration::from_millis(600)).await;
        assert!(!process.is_finished());
        assert_eq!(STEADY.load(Ordering::SeqCst), 1);
        assert_eq!(PANICS.load(Ordering::SeqCst), 2);
        let attempts = BAD_CONFIG.load(Ordering::SeqCst);
        assert!(attempts >= 5, "bad-config was tried {attempts} times");
        let gaps: Vec<Duration> = BAD_CONFIG_AT
            .lock()
            .unwrap()
            .windows(2)
            .map(|pair| pair[1] - pair[0])
            .collect();
        assert!(gaps[0] >= Duration::from_millis(20), "{gaps:?}");
        assert!(gaps[1] >= Duration::from_millis(40), "{gaps:?}");
        assert!(gaps[2] >= Duration::from_millis(80), "{gaps:?}");
        assert!(
            gaps.iter().all(|gap| *gap < Duration::from_millis(200)),
            "{gaps:?}"
        );

        shutdown.cancel();
        tokio::time::timeout(Duration::from_secs(5), process)
            .await
            .expect("the process stops after cancel")
            .unwrap();
        assert_eq!(STEADY_STOPPED.load(Ordering::SeqCst), 1);
        assert_eq!(STEADY.load(Ordering::SeqCst), 1);
    }
}
