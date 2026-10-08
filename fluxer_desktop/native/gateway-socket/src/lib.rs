// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{
    net::{IpAddr, SocketAddr},
    sync::{
        Arc, Mutex, Once, OnceLock,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    time::Duration,
};

use futures_util::{SinkExt, StreamExt};
use napi::{
    Error, Status,
    bindgen_prelude::{Buffer, Function, Result},
    threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
};
use napi_derive::napi;
use tokio::{
    net::TcpStream,
    sync::{Notify, mpsc},
};
use tokio_tungstenite::{
    Connector, MaybeTlsStream, WebSocketStream, client_async_tls_with_config,
    tungstenite::{
        Error as WebSocketError, Message, Utf8Bytes,
        protocol::{CloseFrame, WebSocketConfig, frame::coding::CloseCode},
    },
};

const EVENT_QUEUE_LIMIT: usize = 64;
const EVENT_QUEUE_BYTES_MAX: usize = 16 * 1024 * 1024;
const EVENT_QUEUE_GLOBAL_BYTES_MAX: usize = 64 * 1024 * 1024;
const EVENT_QUEUE_ENTRY_OVERHEAD_BYTES: usize = 256;
const COMMAND_QUEUE_LIMIT: usize = 64;
const COMMAND_QUEUE_BYTES_MAX: usize = 16 * 1024 * 1024;
const COMMAND_QUEUE_GLOBAL_BYTES_MAX: usize = 64 * 1024 * 1024;
const COMMAND_QUEUE_ENTRY_OVERHEAD_BYTES: usize = 128;
const TRANSPORT_RUNTIME_WORKER_THREADS: usize = 4;
const TRANSPORT_RUNTIME_BLOCKING_THREADS_MAX: usize = 4;
const ENDPOINT_URL_MAX_BYTES: usize = 2_048;
const PINNED_ADDRESS_MAX_BYTES: usize = 64;
const GATEWAY_FRAME_MAX_BYTES: usize = 8 * 1024 * 1024;
const VOICE_FRAME_MAX_BYTES: usize = 256 * 1024;
const CLOSE_REASON_MAX_BYTES: usize = 123;
const CONNECT_TIMEOUT: Duration = Duration::from_secs(10);
const SOCKET_WRITE_TIMEOUT: Duration = Duration::from_secs(10);
const DISPOSE_CLOSE_TIMEOUT: Duration = Duration::from_millis(500);

const EVENT_KIND_OPEN: &str = "open";
const EVENT_KIND_MESSAGE: &str = "message";
const EVENT_KIND_BINARY: &str = "binary";
const EVENT_KIND_ERROR: &str = "error";
const EVENT_KIND_CLOSE: &str = "close";

const CLOSE_REASON_DISPOSED: &str = "Disposing native gateway socket";
const CLOSE_REASON_DROPPED: &str = "Native gateway socket dropped";
const CLOSE_REASON_FRAME_TOO_LARGE: &str = "Gateway frame too large";
const CLOSE_REASON_DELIVERY_FAILED: &str = "Gateway event delivery failed";

const _: () =
    assert!(GATEWAY_FRAME_MAX_BYTES + EVENT_QUEUE_ENTRY_OVERHEAD_BYTES <= EVENT_QUEUE_BYTES_MAX);
const _: () = assert!(
    GATEWAY_FRAME_MAX_BYTES + COMMAND_QUEUE_ENTRY_OVERHEAD_BYTES <= COMMAND_QUEUE_BYTES_MAX
);

static RUSTLS_PROVIDER_INIT: Once = Once::new();
static TRANSPORT_RUNTIME: OnceLock<std::result::Result<tokio::runtime::Runtime, String>> =
    OnceLock::new();
static TLS_CLIENT_CONFIG: OnceLock<std::result::Result<Arc<rustls::ClientConfig>, String>> =
    OnceLock::new();
static EVENT_QUEUE_GLOBAL_BYTES: AtomicUsize = AtomicUsize::new(0);
static COMMAND_QUEUE_GLOBAL_BYTES: AtomicUsize = AtomicUsize::new(0);
static EVENT_QUEUE_GLOBAL_RELEASED: Notify = Notify::const_new();
static COMMAND_QUEUE_GLOBAL_RELEASED: Notify = Notify::const_new();

struct QueueBudget {
    queued_entries: AtomicUsize,
    queued_bytes: AtomicUsize,
    entry_limit: usize,
    byte_limit: usize,
    global_bytes: &'static AtomicUsize,
    global_byte_limit: usize,
    capacity_released: Notify,
    global_released: &'static Notify,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct QueueCapacityExceeded;

impl QueueBudget {
    fn new(
        entry_limit: usize,
        byte_limit: usize,
        global_bytes: &'static AtomicUsize,
        global_byte_limit: usize,
        global_released: &'static Notify,
    ) -> Self {
        Self {
            queued_entries: AtomicUsize::new(0),
            queued_bytes: AtomicUsize::new(0),
            entry_limit,
            byte_limit,
            global_bytes,
            global_byte_limit,
            capacity_released: Notify::new(),
            global_released,
        }
    }

    fn wake_capacity_waiters(&self) {
        self.capacity_released.notify_waiters();
    }

    fn reserve(
        self: &Arc<Self>,
        bytes: usize,
    ) -> std::result::Result<QueueReservation, QueueCapacityExceeded> {
        if !try_reserve_atomic(&self.queued_entries, 1, self.entry_limit) {
            return Err(QueueCapacityExceeded);
        }
        if !try_reserve_atomic(&self.queued_bytes, bytes, self.byte_limit) {
            self.queued_entries.fetch_sub(1, Ordering::AcqRel);
            return Err(QueueCapacityExceeded);
        }
        if !try_reserve_atomic(self.global_bytes, bytes, self.global_byte_limit) {
            self.queued_bytes.fetch_sub(bytes, Ordering::AcqRel);
            self.queued_entries.fetch_sub(1, Ordering::AcqRel);
            return Err(QueueCapacityExceeded);
        }
        Ok(QueueReservation {
            budget: self.clone(),
            bytes,
        })
    }
}

struct QueueReservation {
    budget: Arc<QueueBudget>,
    bytes: usize,
}

impl Drop for QueueReservation {
    fn drop(&mut self) {
        let global_bytes = self
            .budget
            .global_bytes
            .fetch_sub(self.bytes, Ordering::AcqRel);
        let local_bytes = self
            .budget
            .queued_bytes
            .fetch_sub(self.bytes, Ordering::AcqRel);
        let local_entries = self.budget.queued_entries.fetch_sub(1, Ordering::AcqRel);
        assert!(global_bytes >= self.bytes);
        assert!(local_bytes >= self.bytes);
        assert!(local_entries > 0);
        self.budget.wake_capacity_waiters();
        self.budget.global_released.notify_waiters();
    }
}

fn try_reserve_atomic(counter: &AtomicUsize, amount: usize, maximum: usize) -> bool {
    let mut current = counter.load(Ordering::Acquire);
    loop {
        let Some(next) = current.checked_add(amount) else {
            return false;
        };
        if next > maximum {
            return false;
        }
        match counter.compare_exchange_weak(current, next, Ordering::AcqRel, Ordering::Acquire) {
            Ok(_) => return true,
            Err(observed) => current = observed,
        }
    }
}

struct TransportEvent {
    kind: &'static str,
    data: Option<String>,
    binary: Option<Vec<u8>>,
    code: Option<u16>,
    reason: Option<String>,
    was_clean: Option<bool>,
    message: Option<String>,
}

impl TransportEvent {
    fn kind(kind: &'static str) -> Self {
        Self {
            kind,
            data: None,
            binary: None,
            code: None,
            reason: None,
            was_clean: None,
            message: None,
        }
    }

    fn message(data: String) -> Self {
        Self {
            data: Some(data),
            ..Self::kind(EVENT_KIND_MESSAGE)
        }
    }

    fn binary(data: Vec<u8>) -> Self {
        Self {
            binary: Some(data),
            ..Self::kind(EVENT_KIND_BINARY)
        }
    }

    fn error(message: String) -> Self {
        Self {
            message: Some(message),
            ..Self::kind(EVENT_KIND_ERROR)
        }
    }

    fn close(code: u16, reason: &str, was_clean: bool) -> Self {
        Self {
            code: Some(code),
            reason: Some(reason.to_owned()),
            was_clean: Some(was_clean),
            ..Self::kind(EVENT_KIND_CLOSE)
        }
    }

    fn queue_bytes(&self) -> usize {
        let mut bytes = EVENT_QUEUE_ENTRY_OVERHEAD_BYTES.saturating_add(self.kind.len());
        for value in [
            self.data.as_deref(),
            self.reason.as_deref(),
            self.message.as_deref(),
        ]
        .into_iter()
        .flatten()
        {
            bytes = bytes.saturating_add(value.len());
        }
        if let Some(binary) = &self.binary {
            bytes = bytes.saturating_add(binary.len());
        }
        bytes
    }
}

struct QueuedTransportEvent {
    event: TransportEvent,
    reservation: QueueReservation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TransportMode {
    Gateway,
    Voice,
}

impl TransportMode {
    fn parse(value: &str) -> Result<Self> {
        match value {
            "gateway" => Ok(Self::Gateway),
            "voice" => Ok(Self::Voice),
            other => Err(invalid_arg_error(format!(
                "unsupported transport mode '{other}', expected 'gateway' or 'voice'"
            ))),
        }
    }

    fn max_frame_bytes(self) -> usize {
        match self {
            Self::Gateway => GATEWAY_FRAME_MAX_BYTES,
            Self::Voice => VOICE_FRAME_MAX_BYTES,
        }
    }

    fn read_capacity_bytes(self) -> usize {
        self.max_frame_bytes().saturating_add(1)
    }
}

enum TransportCommand {
    Send(Message),
    Close { code: u16, reason: String },
}

impl TransportCommand {
    fn queue_bytes(&self) -> usize {
        let payload_bytes = match self {
            Self::Send(message) => message.len(),
            Self::Close { reason, .. } => reason.len(),
        };
        COMMAND_QUEUE_ENTRY_OVERHEAD_BYTES.saturating_add(payload_bytes)
    }
}

struct QueuedTransportCommand {
    command: TransportCommand,
    reservation: QueueReservation,
}

trait EventDelivery: Send + Sync {
    fn deliver(&self, event: QueuedTransportEvent) -> bool;
    fn deliver_terminal(&self, event: TransportEvent);
}

struct EventSink {
    delivery: Arc<dyn EventDelivery>,
    queue_budget: Arc<QueueBudget>,
    disposed: Arc<AtomicBool>,
    delivery_failed: AtomicBool,
}

impl EventSink {
    async fn emit(&self, event: TransportEvent) -> bool {
        let Some(reservation) = self.reserve_when_ready(event.queue_bytes()).await else {
            return false;
        };
        if self
            .delivery
            .deliver(QueuedTransportEvent { event, reservation })
        {
            return true;
        }
        self.fail_delivery();
        false
    }

    async fn reserve_when_ready(&self, bytes: usize) -> Option<QueueReservation> {
        loop {
            let local_released = self.queue_budget.capacity_released.notified();
            let global_released = self.queue_budget.global_released.notified();
            tokio::pin!(local_released, global_released);
            local_released.as_mut().enable();
            global_released.as_mut().enable();
            if self.disposed.load(Ordering::Acquire) || self.delivery_failed.load(Ordering::Acquire)
            {
                return None;
            }
            if let Ok(reservation) = self.queue_budget.reserve(bytes) {
                return Some(reservation);
            }
            tokio::select! {
                () = local_released => {}
                () = global_released => {}
            }
        }
    }

    fn fail_delivery(&self) {
        if self.delivery_failed.swap(true, Ordering::AcqRel) {
            return;
        }
        self.delivery.deliver_terminal(TransportEvent::close(
            1006,
            CLOSE_REASON_DELIVERY_FAILED,
            false,
        ));
    }

    async fn emit_message(&self, data: String) -> bool {
        self.emit(TransportEvent::message(data)).await
    }

    async fn emit_binary(&self, data: Vec<u8>) -> bool {
        self.emit(TransportEvent::binary(data)).await
    }

    async fn emit_error(&self, message: impl Into<String>) {
        self.emit(TransportEvent::error(message.into())).await;
    }

    async fn emit_close(&self, code: u16, reason: &str, was_clean: bool) {
        self.emit(TransportEvent::close(code, reason, was_clean))
            .await;
    }
}

struct ConnectionTarget {
    url: String,
    host: String,
    port: u16,
    tls: bool,
    pinned_address: Option<IpAddr>,
}

struct CommandChannel {
    sender: mpsc::Sender<QueuedTransportCommand>,
    queue_budget: Arc<QueueBudget>,
    mode: TransportMode,
}

struct Inner {
    command_channel: Mutex<Option<CommandChannel>>,
    event_queue_budget: Arc<QueueBudget>,
    disposed: Arc<AtomicBool>,
    dispose_notify: Arc<Notify>,
}

struct TransportSession {
    target: ConnectionTarget,
    mode: TransportMode,
    command_rx: mpsc::Receiver<QueuedTransportCommand>,
    sink: EventSink,
    disposed: Arc<AtomicBool>,
    dispose_notify: Arc<Notify>,
}

#[napi(js_name = "NativeGatewayConnection")]
pub struct NativeGatewayConnection {
    inner: Arc<Inner>,
}

#[napi]
impl NativeGatewayConnection {
    #[napi(js_name = "sendText")]
    pub fn send_text(&self, text: String) -> Result<()> {
        self.send_frame(Message::Text(Utf8Bytes::from(text)))
    }

    #[napi(js_name = "sendBinary")]
    pub fn send_binary(&self, payload: Buffer) -> Result<()> {
        self.send_frame(Message::Binary(payload.to_vec().into()))
    }

    #[napi]
    pub fn close(&self, code: u32, reason: String) -> Result<()> {
        let code = validate_close_code(code)?;
        validate_close_reason(&reason)?;
        self.send_command(TransportCommand::Close { code, reason })
    }

    #[napi]
    pub fn dispose(&self) -> Result<()> {
        if self.inner.disposed.swap(true, Ordering::AcqRel) {
            return Ok(());
        }
        self.inner.dispose_notify.notify_one();
        self.inner.event_queue_budget.wake_capacity_waiters();
        self.inner
            .command_channel
            .lock()
            .map_err(|_| generic_error("gateway socket lock poisoned"))?
            .take();
        Ok(())
    }
}

impl NativeGatewayConnection {
    fn start(
        target: ConnectionTarget,
        mode: TransportMode,
        delivery: Arc<dyn EventDelivery>,
    ) -> Result<Self> {
        let runtime = transport_runtime()?;
        let disposed = Arc::new(AtomicBool::new(false));
        let dispose_notify = Arc::new(Notify::new());
        let (command_tx, command_rx) = mpsc::channel(COMMAND_QUEUE_LIMIT);
        let event_queue_budget = Arc::new(QueueBudget::new(
            EVENT_QUEUE_LIMIT,
            EVENT_QUEUE_BYTES_MAX,
            &EVENT_QUEUE_GLOBAL_BYTES,
            EVENT_QUEUE_GLOBAL_BYTES_MAX,
            &EVENT_QUEUE_GLOBAL_RELEASED,
        ));
        let sink = EventSink {
            delivery,
            queue_budget: event_queue_budget.clone(),
            disposed: disposed.clone(),
            delivery_failed: AtomicBool::new(false),
        };
        runtime.spawn(run_connection(TransportSession {
            target,
            mode,
            command_rx,
            sink,
            disposed: disposed.clone(),
            dispose_notify: dispose_notify.clone(),
        }));
        Ok(Self {
            inner: Arc::new(Inner {
                command_channel: Mutex::new(Some(CommandChannel {
                    sender: command_tx,
                    queue_budget: Arc::new(QueueBudget::new(
                        COMMAND_QUEUE_LIMIT,
                        COMMAND_QUEUE_BYTES_MAX,
                        &COMMAND_QUEUE_GLOBAL_BYTES,
                        COMMAND_QUEUE_GLOBAL_BYTES_MAX,
                        &COMMAND_QUEUE_GLOBAL_RELEASED,
                    )),
                    mode,
                })),
                event_queue_budget,
                disposed,
                dispose_notify,
            }),
        })
    }

    fn send_frame(&self, message: Message) -> Result<()> {
        let mode = self.connected_mode()?;
        if message.len() > mode.max_frame_bytes() {
            return Err(invalid_arg_error(format!(
                "gateway socket payload exceeds {} bytes",
                mode.max_frame_bytes()
            )));
        }
        self.send_command(TransportCommand::Send(message))
    }

    fn connected_mode(&self) -> Result<TransportMode> {
        self.inner
            .command_channel
            .lock()
            .map_err(|_| generic_error("gateway socket lock poisoned"))?
            .as_ref()
            .map(|channel| channel.mode)
            .ok_or_else(|| generic_error("gateway socket is disposed"))
    }

    fn send_command(&self, command: TransportCommand) -> Result<()> {
        if self.inner.disposed.load(Ordering::Acquire) {
            return Err(generic_error("gateway socket is disposed"));
        }
        let (sender, queue_budget) = self
            .inner
            .command_channel
            .lock()
            .map_err(|_| generic_error("gateway socket lock poisoned"))?
            .as_ref()
            .map(|channel| (channel.sender.clone(), channel.queue_budget.clone()))
            .ok_or_else(|| generic_error("gateway socket is disposed"))?;
        let reservation = queue_budget
            .reserve(command.queue_bytes())
            .map_err(|_| generic_error("gateway socket command queue capacity reached"))?;
        sender
            .try_send(QueuedTransportCommand {
                command,
                reservation,
            })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => {
                    generic_error("gateway socket command queue is full")
                }
                mpsc::error::TrySendError::Closed(_) => {
                    generic_error("gateway socket connection task is closed")
                }
            })
    }
}

impl Drop for NativeGatewayConnection {
    fn drop(&mut self) {
        let _ = self.dispose();
    }
}

fn parse_websocket_connection_target(
    url: &str,
    pinned_address: Option<&str>,
) -> Result<ConnectionTarget> {
    if url.is_empty()
        || url.len() > ENDPOINT_URL_MAX_BYTES
        || url.trim() != url
        || url.chars().any(char::is_control)
    {
        return Err(invalid_arg_error("gateway socket connect URL is invalid"));
    }
    let parsed_url = url::Url::parse(url)
        .map_err(|_| invalid_arg_error("gateway socket connect URL is invalid"))?;
    if parsed_url.scheme() != "ws" && parsed_url.scheme() != "wss" {
        return Err(invalid_arg_error(
            "gateway socket connect URL must use ws or wss",
        ));
    }
    if !parsed_url.username().is_empty()
        || parsed_url.password().is_some()
        || parsed_url.fragment().is_some()
    {
        return Err(invalid_arg_error(
            "gateway socket connect URL authority is invalid",
        ));
    }
    let host = match parsed_url.host() {
        Some(url::Host::Domain(domain)) => domain.to_owned(),
        Some(url::Host::Ipv4(address)) => address.to_string(),
        Some(url::Host::Ipv6(address)) => address.to_string(),
        None => {
            return Err(invalid_arg_error(
                "gateway socket connect URL authority is invalid",
            ));
        }
    };
    let port = parsed_url
        .port_or_known_default()
        .ok_or_else(|| invalid_arg_error("gateway socket connect URL port is invalid"))?;
    Ok(ConnectionTarget {
        url: url.to_owned(),
        host,
        port,
        tls: parsed_url.scheme() == "wss",
        pinned_address: parse_pinned_address(pinned_address)?,
    })
}

fn parse_pinned_address(pinned_address: Option<&str>) -> Result<Option<IpAddr>> {
    let Some(pinned_address) = pinned_address else {
        return Ok(None);
    };
    if pinned_address.is_empty()
        || pinned_address.len() > PINNED_ADDRESS_MAX_BYTES
        || pinned_address.trim() != pinned_address
        || pinned_address.chars().any(char::is_control)
    {
        return Err(invalid_arg_error(
            "gateway socket pinned address is invalid",
        ));
    }
    pinned_address
        .parse::<IpAddr>()
        .map(Some)
        .map_err(|_| invalid_arg_error("gateway socket pinned address must be an IP address"))
}

fn ensure_rustls_crypto_provider() {
    RUSTLS_PROVIDER_INIT.call_once(|| {
        if rustls::crypto::CryptoProvider::get_default().is_none() {
            let _ = rustls::crypto::ring::default_provider().install_default();
        }
        assert!(
            rustls::crypto::CryptoProvider::get_default().is_some(),
            "rustls CryptoProvider must be installed before gateway socket TLS use"
        );
    });
}

fn build_tls_client_config() -> std::result::Result<Arc<rustls::ClientConfig>, String> {
    ensure_rustls_crypto_provider();
    let native = rustls_native_certs::load_native_certs();
    if native.certs.is_empty() {
        return Err(format!(
            "no native root CA certificates found (errors: {:?})",
            native.errors
        ));
    }
    let mut roots = rustls::RootCertStore::empty();
    roots.add_parsable_certificates(native.certs);
    Ok(Arc::new(
        rustls::ClientConfig::builder()
            .with_root_certificates(roots)
            .with_no_client_auth(),
    ))
}

fn tls_client_config() -> std::result::Result<Arc<rustls::ClientConfig>, String> {
    TLS_CLIENT_CONFIG
        .get_or_init(build_tls_client_config)
        .clone()
}

async fn tls_connector_for(
    target: &ConnectionTarget,
) -> std::result::Result<Option<Connector>, WebSocketError> {
    if !target.tls {
        return Ok(None);
    }
    let config = match TLS_CLIENT_CONFIG.get() {
        Some(config) => config.clone(),
        None => tokio::task::spawn_blocking(tls_client_config)
            .await
            .map_err(|error| WebSocketError::Io(std::io::Error::other(error.to_string())))?,
    };
    config
        .map(|config| Some(Connector::Rustls(config)))
        .map_err(|error| {
            WebSocketError::Io(std::io::Error::new(std::io::ErrorKind::NotFound, error))
        })
}

fn transport_runtime() -> Result<&'static tokio::runtime::Runtime> {
    match TRANSPORT_RUNTIME.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(TRANSPORT_RUNTIME_WORKER_THREADS)
            .max_blocking_threads(TRANSPORT_RUNTIME_BLOCKING_THREADS_MAX)
            .thread_name("fluxer-gateway-socket")
            .enable_all()
            .build()
            .map_err(|error| error.to_string())
    }) {
        Ok(runtime) => Ok(runtime),
        Err(error) => Err(generic_error(format!(
            "gateway socket runtime initialization failed: {error}"
        ))),
    }
}

async fn connect_socket(
    target: &ConnectionTarget,
    config: WebSocketConfig,
) -> std::result::Result<WebSocketStream<MaybeTlsStream<TcpStream>>, WebSocketError> {
    let tcp = async {
        match target.pinned_address {
            Some(address) => TcpStream::connect(SocketAddr::new(address, target.port)).await,
            None => TcpStream::connect((target.host.as_str(), target.port)).await,
        }
        .map_err(WebSocketError::Io)
    };
    let (stream, connector) = tokio::try_join!(tcp, tls_connector_for(target))?;
    stream.set_nodelay(true).map_err(WebSocketError::Io)?;
    let (socket, _response) =
        client_async_tls_with_config(target.url.as_str(), stream, Some(config), connector).await?;
    Ok(socket)
}

async fn run_connection(session: TransportSession) {
    let TransportSession {
        target,
        mode,
        mut command_rx,
        sink,
        disposed,
        dispose_notify,
    } = session;

    let websocket_config = WebSocketConfig::default()
        .write_buffer_size(0)
        .max_write_buffer_size(mode.max_frame_bytes().saturating_mul(2))
        .max_message_size(Some(mode.read_capacity_bytes()))
        .max_frame_size(Some(mode.read_capacity_bytes()));

    if disposed.load(Ordering::Acquire) {
        return;
    }
    let connect_result = tokio::select! {
        biased;
        _ = dispose_notify.notified() => return,
        result = tokio::time::timeout(CONNECT_TIMEOUT, connect_socket(&target, websocket_config)) => result,
    };
    let socket = match connect_result {
        Ok(Ok(socket)) => socket,
        Ok(Err(error)) => {
            sink.emit_error(format!("gateway websocket connect failed: {error}"))
                .await;
            sink.emit_close(1006, "Gateway websocket connect failed", false)
                .await;
            return;
        }
        Err(_) => {
            sink.emit_error("gateway websocket connect timed out").await;
            sink.emit_close(1006, "Gateway websocket connect timed out", false)
                .await;
            return;
        }
    };
    if !sink.emit(TransportEvent::kind(EVENT_KIND_OPEN)).await {
        return;
    }

    let (mut write, mut read) = socket.split();

    loop {
        tokio::select! {
            biased;
            _ = dispose_notify.notified() => {
                let _ = send_close_frame_with_timeout(
                    &mut write,
                    1000,
                    CLOSE_REASON_DISPOSED,
                    DISPOSE_CLOSE_TIMEOUT,
                )
                .await;
                return;
            }
            queued_command = command_rx.recv() => {
                let Some(QueuedTransportCommand {command, reservation: _reservation}) = queued_command else {
                    let _ = send_close_frame_with_timeout(
                        &mut write,
                        1000,
                        CLOSE_REASON_DROPPED,
                        DISPOSE_CLOSE_TIMEOUT,
                    )
                    .await;
                    return;
                };
                if disposed.load(Ordering::Acquire) {
                    let _ = send_close_frame_with_timeout(
                        &mut write,
                        1000,
                        CLOSE_REASON_DISPOSED,
                        DISPOSE_CLOSE_TIMEOUT,
                    )
                    .await;
                    return;
                }
                match command {
                    TransportCommand::Send(message) => {
                        match tokio::time::timeout(SOCKET_WRITE_TIMEOUT, write.send(message)).await {
                            Ok(Ok(())) => {}
                            Ok(Err(error)) => {
                                sink.emit_error(format!("gateway websocket send failed: {error}")).await;
                                sink.emit_close(1006, "Gateway websocket send failed", false).await;
                                return;
                            }
                            Err(_) => {
                                sink.emit_error("gateway websocket send timed out").await;
                                sink.emit_close(1006, "Gateway websocket send timed out", false).await;
                                return;
                            }
                        }
                    }
                    TransportCommand::Close {code, reason} => {
                        if let Err(error) = send_close_frame(&mut write, code, reason.as_str()).await {
                            sink.emit_error(format!("gateway websocket close failed: {error}")).await;
                            sink.emit_close(1006, "Gateway websocket close failed", false).await;
                            return;
                        }
                        sink.emit_close(code, reason.as_str(), true).await;
                        return;
                    }
                }
            }
            message = read.next() => {
                match message {
                    Some(Ok(message)) => match handle_socket_message(&sink, mode, message).await {
                        SocketMessageOutcome::Continue => {}
                        SocketMessageOutcome::Fail {code, reason} => {
                            let _ = send_close_frame(&mut write, code, reason).await;
                            return;
                        }
                        SocketMessageOutcome::PeerClosed => {
                            let _ = tokio::time::timeout(SOCKET_WRITE_TIMEOUT, write.close()).await;
                            return;
                        }
                        SocketMessageOutcome::DeliveryFailed => {
                            let _ = send_close_frame(&mut write, 1011, CLOSE_REASON_DELIVERY_FAILED).await;
                            return;
                        }
                    },
                    Some(Err(error)) => {
                        sink.emit_error(format!("gateway websocket read failed: {error}")).await;
                        sink.emit_close(1006, "Gateway websocket read failed", false).await;
                        return;
                    }
                    None => {
                        sink.emit_close(1006, "Gateway websocket stream ended", false).await;
                        return;
                    }
                }
            }
        }
    }
}

async fn send_close_frame<S>(
    write: &mut S,
    code: u16,
    reason: &str,
) -> std::result::Result<(), String>
where
    S: futures_util::Sink<Message, Error = WebSocketError> + Unpin,
{
    send_close_frame_with_timeout(write, code, reason, SOCKET_WRITE_TIMEOUT).await
}

async fn send_close_frame_with_timeout<S>(
    write: &mut S,
    code: u16,
    reason: &str,
    timeout: Duration,
) -> std::result::Result<(), String>
where
    S: futures_util::Sink<Message, Error = WebSocketError> + Unpin,
{
    match tokio::time::timeout(
        timeout,
        write.send(Message::Close(Some(CloseFrame {
            code: CloseCode::from(code),
            reason: Utf8Bytes::from(reason.to_owned()),
        }))),
    )
    .await
    {
        Ok(Ok(())) => Ok(()),
        Ok(Err(error)) => Err(error.to_string()),
        Err(_) => Err("WebSocket close send timed out".to_owned()),
    }
}

enum SocketMessageOutcome {
    Continue,
    Fail { code: u16, reason: &'static str },
    PeerClosed,
    DeliveryFailed,
}

fn delivered(delivered: bool) -> SocketMessageOutcome {
    if delivered {
        SocketMessageOutcome::Continue
    } else {
        SocketMessageOutcome::DeliveryFailed
    }
}

async fn reject_over_size_frame(sink: &EventSink, frame_kind: &str) -> SocketMessageOutcome {
    sink.emit_error(format!(
        "gateway websocket {frame_kind} frame exceeds its bounded maximum"
    ))
    .await;
    sink.emit_close(1009, CLOSE_REASON_FRAME_TOO_LARGE, false)
        .await;
    SocketMessageOutcome::Fail {
        code: 1009,
        reason: CLOSE_REASON_FRAME_TOO_LARGE,
    }
}

async fn handle_socket_message(
    sink: &EventSink,
    mode: TransportMode,
    message: Message,
) -> SocketMessageOutcome {
    match message {
        Message::Text(text) => {
            if text.len() > mode.max_frame_bytes() {
                return reject_over_size_frame(sink, "text").await;
            }
            delivered(sink.emit_message(text.to_string()).await)
        }
        Message::Binary(bytes) => {
            if bytes.len() > mode.max_frame_bytes() {
                return reject_over_size_frame(sink, "binary").await;
            }
            delivered(sink.emit_binary(bytes.to_vec()).await)
        }
        Message::Close(frame) => {
            let (code, reason) = frame
                .map(|frame| (u16::from(frame.code), frame.reason.to_string()))
                .unwrap_or((1005, String::new()));
            sink.emit_close(code, reason.as_str(), true).await;
            SocketMessageOutcome::PeerClosed
        }
        Message::Ping(_) | Message::Pong(_) | Message::Frame(_) => SocketMessageOutcome::Continue,
    }
}

fn validate_close_code(code: u32) -> Result<u16> {
    let code = u16::try_from(code)
        .map_err(|_| invalid_arg_error("WebSocket close code is out of range"))?;
    if !CloseCode::from(code).is_allowed() {
        return Err(invalid_arg_error(format!(
            "WebSocket close code {code} is not allowed"
        )));
    }
    Ok(code)
}

fn validate_close_reason(reason: &str) -> Result<()> {
    if reason.len() > CLOSE_REASON_MAX_BYTES {
        return Err(invalid_arg_error(format!(
            "WebSocket close reason exceeds {CLOSE_REASON_MAX_BYTES} bytes"
        )));
    }
    Ok(())
}

fn invalid_arg_error(message: impl Into<String>) -> Error {
    Error::new(Status::InvalidArg, message.into())
}

fn generic_error(message: impl Into<String>) -> Error {
    Error::new(Status::GenericFailure, message.into())
}

#[napi(object, use_nullable = true)]
pub struct NativeGatewayEvent {
    pub kind: String,
    pub data: Option<String>,
    pub binary: Option<Buffer>,
    pub code: Option<u16>,
    pub reason: Option<String>,
    #[napi(js_name = "wasClean")]
    pub was_clean: Option<bool>,
    pub message: Option<String>,
}

impl From<TransportEvent> for NativeGatewayEvent {
    fn from(event: TransportEvent) -> Self {
        Self {
            kind: event.kind.to_owned(),
            data: event.data,
            binary: event.binary.map(Buffer::from),
            code: event.code,
            reason: event.reason,
            was_clean: event.was_clean,
            message: event.message,
        }
    }
}

#[napi(object, use_nullable = true)]
pub struct NativeGatewayConnectOptions {
    pub url: String,
    pub address: Option<String>,
    pub mode: String,
}

type EventThreadsafeFunction = ThreadsafeFunction<
    QueuedTransportEvent,
    UnknownReturnValue,
    NativeGatewayEvent,
    Status,
    false,
    true,
    0,
>;

type TerminalEventThreadsafeFunction = ThreadsafeFunction<
    TransportEvent,
    UnknownReturnValue,
    NativeGatewayEvent,
    Status,
    false,
    true,
    0,
>;

struct NapiEventDelivery {
    regular: EventThreadsafeFunction,
    terminal: TerminalEventThreadsafeFunction,
}

impl EventDelivery for NapiEventDelivery {
    fn deliver(&self, event: QueuedTransportEvent) -> bool {
        self.regular
            .call(event, ThreadsafeFunctionCallMode::NonBlocking)
            == Status::Ok
    }

    fn deliver_terminal(&self, event: TransportEvent) {
        let _ = self
            .terminal
            .call(event, ThreadsafeFunctionCallMode::NonBlocking);
    }
}

#[napi]
pub fn warmup() -> Result<()> {
    let runtime = transport_runtime()?;
    if TLS_CLIENT_CONFIG.get().is_none() {
        runtime.spawn_blocking(tls_client_config);
    }
    Ok(())
}

#[napi]
pub fn connect(
    options: NativeGatewayConnectOptions,
    on_event: Function<NativeGatewayEvent, UnknownReturnValue>,
    on_terminal_event: Function<NativeGatewayEvent, UnknownReturnValue>,
) -> Result<NativeGatewayConnection> {
    let mode = TransportMode::parse(&options.mode)?;
    let target = parse_websocket_connection_target(&options.url, options.address.as_deref())?;
    ensure_rustls_crypto_provider();
    let regular = on_event
        .build_threadsafe_function::<QueuedTransportEvent>()
        .weak::<true>()
        .callee_handled::<false>()
        .max_queue_size::<0>()
        .build_callback(|context| {
            let QueuedTransportEvent { event, reservation } = context.value;
            drop(reservation);
            Ok(NativeGatewayEvent::from(event))
        })
        .map_err(|error| {
            generic_error(format!(
                "failed to create gateway socket callback: {}",
                error.reason
            ))
        })?;
    let terminal = on_terminal_event
        .build_threadsafe_function::<TransportEvent>()
        .weak::<true>()
        .callee_handled::<false>()
        .max_queue_size::<0>()
        .build_callback(|context| Ok(NativeGatewayEvent::from(context.value)))
        .map_err(|error| {
            generic_error(format!(
                "failed to create gateway socket terminal callback: {}",
                error.reason
            ))
        })?;
    NativeGatewayConnection::start(
        target,
        mode,
        Arc::new(NapiEventDelivery { regular, terminal }),
    )
}

#[cfg(test)]
mod tests {
    use std::{future::Future, time::Instant};

    use tokio::net::TcpListener;
    use tokio_tungstenite::accept_async;

    use super::*;

    const EVENT_WAIT_TIMEOUT: Duration = Duration::from_secs(10);
    const STALLED_CONSUMER_SETTLE: Duration = Duration::from_millis(500);

    struct RecordingDelivery {
        events: mpsc::UnboundedSender<TransportEvent>,
    }

    impl EventDelivery for RecordingDelivery {
        fn deliver(&self, event: QueuedTransportEvent) -> bool {
            let QueuedTransportEvent { event, reservation } = event;
            drop(reservation);
            self.events.send(event).is_ok()
        }

        fn deliver_terminal(&self, event: TransportEvent) {
            let _ = self.events.send(event);
        }
    }

    struct StallingDelivery {
        events: mpsc::UnboundedSender<TransportEvent>,
        held: Mutex<Option<Vec<QueueReservation>>>,
    }

    impl StallingDelivery {
        fn resume(&self) {
            self.held.lock().expect("stalled reservations").take();
        }
    }

    impl EventDelivery for StallingDelivery {
        fn deliver(&self, event: QueuedTransportEvent) -> bool {
            let QueuedTransportEvent { event, reservation } = event;
            match self.held.lock().expect("stalled reservations").as_mut() {
                Some(held) => held.push(reservation),
                None => drop(reservation),
            }
            self.events.send(event).is_ok()
        }

        fn deliver_terminal(&self, event: TransportEvent) {
            let _ = self.events.send(event);
        }
    }

    fn spawn_server<Handler, Served>(handler: Handler) -> SocketAddr
    where
        Handler: FnOnce(WebSocketStream<TcpStream>) -> Served + Send + 'static,
        Served: Future<Output = ()> + Send,
    {
        let runtime = transport_runtime().expect("transport runtime");
        let listener = runtime
            .block_on(TcpListener::bind("127.0.0.1:0"))
            .expect("bind test listener");
        let address = listener.local_addr().expect("listener address");
        runtime.spawn(async move {
            let (stream, _) = listener.accept().await.expect("accept test connection");
            let socket = accept_async(stream).await.expect("accept websocket");
            handler(socket).await;
        });
        address
    }

    fn spawn_echo_server() -> SocketAddr {
        spawn_server(|mut socket| async move {
            while let Some(Ok(message)) = socket.next().await {
                match message {
                    Message::Text(_) | Message::Binary(_) => {
                        if socket.send(message).await.is_err() {
                            return;
                        }
                    }
                    Message::Close(_) => return,
                    _ => {}
                }
            }
        })
    }

    fn test_connection(
        address: SocketAddr,
        pinned_address: Option<&str>,
        mode: TransportMode,
    ) -> (
        NativeGatewayConnection,
        mpsc::UnboundedReceiver<TransportEvent>,
    ) {
        let (events, receiver) = mpsc::unbounded_channel();
        let target = parse_websocket_connection_target(
            &format!("ws://127.0.0.1:{}/", address.port()),
            pinned_address,
        )
        .expect("connection target");
        let connection =
            NativeGatewayConnection::start(target, mode, Arc::new(RecordingDelivery { events }))
                .expect("start connection");
        (connection, receiver)
    }

    fn block_recv<T>(receiver: &mut mpsc::UnboundedReceiver<T>) -> T {
        let runtime = transport_runtime().expect("transport runtime");
        runtime.block_on(async {
            tokio::time::timeout(EVENT_WAIT_TIMEOUT, receiver.recv())
                .await
                .expect("timed out waiting for a value")
                .expect("value channel closed")
        })
    }

    fn next_event(events: &mut mpsc::UnboundedReceiver<TransportEvent>) -> TransportEvent {
        block_recv(events)
    }

    fn expect_open(events: &mut mpsc::UnboundedReceiver<TransportEvent>) {
        let event = next_event(events);
        assert_eq!(event.kind, EVENT_KIND_OPEN, "{:?}", event.message);
    }

    #[test]
    fn connection_targets_reject_unusable_urls() {
        for url in [
            "",
            "http://gateway.example/",
            "https://gateway.example/",
            " wss://gateway.example/",
            "wss://user:secret@gateway.example/",
            "wss://user@gateway.example/",
            "wss://gateway.example/#fragment",
            "wss://gateway.example/\u{7}",
            "not-a-url",
        ] {
            assert!(
                parse_websocket_connection_target(url, None).is_err(),
                "expected {url} to be rejected"
            );
        }
        let long_url = format!(
            "wss://gateway.example/{}",
            "a".repeat(ENDPOINT_URL_MAX_BYTES)
        );
        assert!(parse_websocket_connection_target(&long_url, None).is_err());
    }

    #[test]
    fn connection_targets_reject_unusable_pinned_addresses() {
        for address in ["", "gateway.example", "127.0.0.1 ", "1.2.3.4\u{7}"] {
            assert!(
                parse_websocket_connection_target("wss://gateway.example/", Some(address)).is_err(),
                "expected {address} to be rejected"
            );
        }
        let long_address = "a".repeat(PINNED_ADDRESS_MAX_BYTES + 1);
        assert!(
            parse_websocket_connection_target("wss://gateway.example/", Some(&long_address))
                .is_err()
        );
    }

    #[test]
    fn connection_targets_resolve_hosts_ports_and_optional_pins() {
        let plain = parse_websocket_connection_target("ws://gateway.example/socket", None).unwrap();
        assert_eq!(plain.host, "gateway.example");
        assert_eq!(plain.port, 80);
        assert!(plain.pinned_address.is_none());
        assert!(!plain.tls);

        let secure =
            parse_websocket_connection_target("wss://gateway.example/socket?v=9", None).unwrap();
        assert_eq!(secure.port, 443);
        assert!(secure.tls);

        let pinned =
            parse_websocket_connection_target("wss://gateway.example:8443/", Some("203.0.113.7"))
                .unwrap();
        assert_eq!(pinned.port, 8443);
        assert_eq!(
            pinned.pinned_address,
            Some("203.0.113.7".parse::<IpAddr>().unwrap())
        );

        let literal = parse_websocket_connection_target("ws://[::1]:9000/", None).unwrap();
        assert_eq!(literal.host, "::1");
        assert_eq!(literal.port, 9000);
    }

    #[test]
    fn every_secure_connection_shares_one_tls_client_config() {
        match (tls_client_config(), tls_client_config()) {
            (Ok(first), Ok(second)) => assert!(Arc::ptr_eq(&first, &second)),
            (Err(first), Err(second)) => assert_eq!(first, second),
            _ => panic!("the cached TLS client config changed between calls"),
        }
    }

    #[tokio::test]
    async fn a_cleartext_target_needs_no_tls_connector() {
        let target = parse_websocket_connection_target("ws://127.0.0.1:9/", None).unwrap();
        assert!(tls_connector_for(&target).await.unwrap().is_none());
    }

    #[test]
    fn transport_modes_parse_and_bound_their_frames() {
        assert_eq!(
            TransportMode::parse("gateway").unwrap().max_frame_bytes(),
            GATEWAY_FRAME_MAX_BYTES
        );
        assert_eq!(
            TransportMode::parse("voice").unwrap().max_frame_bytes(),
            VOICE_FRAME_MAX_BYTES
        );
        assert!(TransportMode::parse("Gateway").is_err());
        assert!(TransportMode::parse("").is_err());
        assert_eq!(
            TransportMode::Gateway.read_capacity_bytes(),
            GATEWAY_FRAME_MAX_BYTES + 1
        );
    }

    #[test]
    fn close_codes_and_reasons_are_validated() {
        assert_eq!(validate_close_code(1000).unwrap(), 1000);
        assert_eq!(validate_close_code(4004).unwrap(), 4004);
        assert!(validate_close_code(1005).is_err());
        assert!(validate_close_code(1006).is_err());
        assert!(validate_close_code(70_000).is_err());
        assert!(validate_close_reason(&"a".repeat(CLOSE_REASON_MAX_BYTES)).is_ok());
        assert!(validate_close_reason(&"a".repeat(CLOSE_REASON_MAX_BYTES + 1)).is_err());
        assert!(validate_close_reason(&"\u{1f600}".repeat(31)).is_err());
    }

    #[test]
    fn the_queue_budget_rejects_past_its_entry_and_byte_caps() {
        static GLOBAL_BYTES: AtomicUsize = AtomicUsize::new(0);
        static GLOBAL_RELEASED: Notify = Notify::const_new();
        let budget = Arc::new(QueueBudget::new(
            2,
            1_024,
            &GLOBAL_BYTES,
            1 << 20,
            &GLOBAL_RELEASED,
        ));

        let first = budget.reserve(16).unwrap();
        let second = budget.reserve(16).unwrap();
        assert!(budget.reserve(16).is_err());

        drop(second);
        assert!(budget.reserve(2_048).is_err());
        assert_eq!(budget.queued_entries.load(Ordering::Acquire), 1);
        assert_eq!(budget.queued_bytes.load(Ordering::Acquire), 16);

        drop(first);
        assert_eq!(budget.queued_entries.load(Ordering::Acquire), 0);
        assert_eq!(budget.queued_bytes.load(Ordering::Acquire), 0);
        assert_eq!(GLOBAL_BYTES.load(Ordering::Acquire), 0);
    }

    #[tokio::test]
    async fn releasing_one_connection_wakes_another_parked_on_the_process_wide_cap() {
        static GLOBAL_BYTES: AtomicUsize = AtomicUsize::new(0);
        static GLOBAL_RELEASED: Notify = Notify::const_new();
        let saturated = Arc::new(QueueBudget::new(
            8,
            1_024,
            &GLOBAL_BYTES,
            1_024,
            &GLOBAL_RELEASED,
        ));
        let parked = Arc::new(QueueBudget::new(
            8,
            1_024,
            &GLOBAL_BYTES,
            1_024,
            &GLOBAL_RELEASED,
        ));

        let held = saturated.reserve(1_024).unwrap();
        assert!(parked.reserve(512).is_err());
        assert_eq!(parked.queued_bytes.load(Ordering::Acquire), 0);

        let waiter = {
            let parked = parked.clone();
            tokio::spawn(async move {
                let released = parked.global_released.notified();
                tokio::pin!(released);
                released.as_mut().enable();
                released.await;
                parked.reserve(512).is_ok()
            })
        };
        tokio::task::yield_now().await;
        drop(held);

        let reserved = tokio::time::timeout(Duration::from_secs(2), waiter)
            .await
            .expect("a connection parked on the global cap must be woken when another releases")
            .expect("the waiting task must not panic");
        assert!(reserved, "the woken connection must be able to reserve");
    }

    #[test]
    fn the_queue_budget_rejects_past_the_process_wide_byte_cap() {
        static GLOBAL_BYTES: AtomicUsize = AtomicUsize::new(0);
        static GLOBAL_RELEASED: Notify = Notify::const_new();
        let first = Arc::new(QueueBudget::new(
            8,
            1_024,
            &GLOBAL_BYTES,
            1_024,
            &GLOBAL_RELEASED,
        ));
        let second = Arc::new(QueueBudget::new(
            8,
            1_024,
            &GLOBAL_BYTES,
            1_024,
            &GLOBAL_RELEASED,
        ));

        let held = first.reserve(768).unwrap();
        assert!(second.reserve(512).is_err());
        let fitting = second.reserve(256).unwrap();
        assert_eq!(GLOBAL_BYTES.load(Ordering::Acquire), 1_024);

        drop(held);
        drop(fitting);
        assert_eq!(GLOBAL_BYTES.load(Ordering::Acquire), 0);
    }

    #[test]
    fn a_burst_past_the_event_queue_limit_waits_for_the_consumer_instead_of_closing() {
        const BURST: usize = EVENT_QUEUE_LIMIT * 2;

        let address = spawn_server(|mut socket| async move {
            for index in 0..BURST {
                if socket
                    .send(Message::Text(Utf8Bytes::from(format!("event-{index}"))))
                    .await
                    .is_err()
                {
                    return;
                }
            }
            while let Some(Ok(message)) = socket.next().await {
                if matches!(message, Message::Close(_)) {
                    return;
                }
            }
        });
        let (events, mut receiver) = mpsc::unbounded_channel();
        let delivery = Arc::new(StallingDelivery {
            events,
            held: Mutex::new(Some(Vec::new())),
        });
        let target =
            parse_websocket_connection_target(&format!("ws://127.0.0.1:{}/", address.port()), None)
                .expect("connection target");
        let _connection =
            NativeGatewayConnection::start(target, TransportMode::Gateway, delivery.clone())
                .expect("start connection");

        expect_open(&mut receiver);
        for index in 0..EVENT_QUEUE_LIMIT - 1 {
            let event = next_event(&mut receiver);
            assert_eq!(event.kind, EVENT_KIND_MESSAGE, "{:?}", event.reason);
            assert_eq!(event.data, Some(format!("event-{index}")));
        }

        let stalled = transport_runtime()
            .expect("transport runtime")
            .block_on(async {
                tokio::time::timeout(STALLED_CONSUMER_SETTLE, receiver.recv()).await
            });
        if let Ok(event) = stalled {
            panic!(
                "a full event queue must stall the reader, not deliver {:?}",
                event.map(|event| (event.kind, event.code, event.reason))
            );
        }

        delivery.resume();
        for index in EVENT_QUEUE_LIMIT - 1..BURST {
            let event = next_event(&mut receiver);
            assert_eq!(event.kind, EVENT_KIND_MESSAGE, "{:?}", event.reason);
            assert_eq!(event.data, Some(format!("event-{index}")));
        }
    }

    #[test]
    fn disposing_while_the_event_queue_is_full_still_closes_the_socket() {
        const BURST: usize = EVENT_QUEUE_LIMIT * 2;

        let (peer_closed, mut peer_closed_events) = mpsc::unbounded_channel();
        let address = spawn_server(move |mut socket| async move {
            for index in 0..BURST {
                if socket
                    .send(Message::Text(Utf8Bytes::from(format!("event-{index}"))))
                    .await
                    .is_err()
                {
                    break;
                }
            }
            while let Some(Ok(message)) = socket.next().await {
                if matches!(message, Message::Close(_)) {
                    break;
                }
            }
            let _ = peer_closed.send(Instant::now());
        });
        let (events, mut receiver) = mpsc::unbounded_channel();
        let delivery = Arc::new(StallingDelivery {
            events,
            held: Mutex::new(Some(Vec::new())),
        });
        let target =
            parse_websocket_connection_target(&format!("ws://127.0.0.1:{}/", address.port()), None)
                .expect("connection target");
        let connection =
            NativeGatewayConnection::start(target, TransportMode::Gateway, delivery.clone())
                .expect("start connection");

        expect_open(&mut receiver);
        for _ in 0..EVENT_QUEUE_LIMIT - 1 {
            let event = next_event(&mut receiver);
            assert_eq!(event.kind, EVENT_KIND_MESSAGE, "{:?}", event.reason);
        }

        drop(connection);
        block_recv(&mut peer_closed_events);
    }

    #[test]
    fn the_echo_server_round_trips_text_and_binary_byte_for_byte() {
        let address = spawn_echo_server();
        let (connection, mut events) =
            test_connection(address, Some("127.0.0.1"), TransportMode::Gateway);
        expect_open(&mut events);

        let text = "héllo \u{1f600} {\"op\":1,\"d\":null}";
        connection.send_text(text.to_owned()).unwrap();
        let event = next_event(&mut events);
        assert_eq!(event.kind, EVENT_KIND_MESSAGE);
        assert_eq!(event.data.as_deref(), Some(text));
        assert!(event.binary.is_none());

        let payload: Vec<u8> = (0..=255u8).chain(0..=255u8).collect();
        connection
            .send_frame(Message::Binary(payload.clone().into()))
            .unwrap();
        let event = next_event(&mut events);
        assert_eq!(event.kind, EVENT_KIND_BINARY);
        assert_eq!(event.binary.as_deref(), Some(payload.as_slice()));
        assert!(event.data.is_none());
    }

    #[test]
    fn closing_reports_a_clean_close_to_both_ends() {
        let (peer_close, mut peer_close_events) = mpsc::unbounded_channel();
        let address = spawn_server(move |mut socket| async move {
            while let Some(Ok(message)) = socket.next().await {
                if let Message::Close(frame) = message {
                    let _ = peer_close
                        .send(frame.map(|frame| {
                            (u16::from(frame.code), frame.reason.as_str().to_owned())
                        }));
                    return;
                }
            }
        });
        let (connection, mut events) = test_connection(address, None, TransportMode::Gateway);
        expect_open(&mut events);

        connection
            .close(4000, "Switching accounts".to_owned())
            .unwrap();
        let event = next_event(&mut events);
        assert_eq!(event.kind, EVENT_KIND_CLOSE);
        assert_eq!(event.code, Some(4000));
        assert_eq!(event.reason.as_deref(), Some("Switching accounts"));
        assert_eq!(event.was_clean, Some(true));
        assert_eq!(
            block_recv(&mut peer_close_events),
            Some((4000, "Switching accounts".to_owned()))
        );
    }

    #[test]
    fn an_over_size_gateway_frame_reports_an_error_and_closes_with_1009() {
        let (peer_close, mut peer_close_events) = mpsc::unbounded_channel();
        let address = spawn_server(move |mut socket| async move {
            let payload = vec![7u8; GATEWAY_FRAME_MAX_BYTES + 1];
            if socket.send(Message::Binary(payload.into())).await.is_err() {
                return;
            }
            while let Some(Ok(message)) = socket.next().await {
                if let Message::Close(frame) = message {
                    let _ = peer_close.send(frame.map(|frame| u16::from(frame.code)));
                    return;
                }
            }
        });
        let (_connection, mut events) = test_connection(address, None, TransportMode::Gateway);
        expect_open(&mut events);

        let error = next_event(&mut events);
        assert_eq!(error.kind, EVENT_KIND_ERROR);
        assert!(error.message.is_some());
        let close = next_event(&mut events);
        assert_eq!(close.kind, EVENT_KIND_CLOSE);
        assert_eq!(close.code, Some(1009));
        assert_eq!(close.was_clean, Some(false));
        assert_eq!(block_recv(&mut peer_close_events), Some(1009));
    }

    #[test]
    fn dropping_the_connection_closes_the_socket_within_the_dispose_timeout() {
        let (peer_closed, mut peer_closed_events) = mpsc::unbounded_channel();
        let address = spawn_server(move |mut socket| async move {
            while let Some(Ok(message)) = socket.next().await {
                if matches!(message, Message::Close(_)) {
                    break;
                }
            }
            let _ = peer_closed.send(Instant::now());
        });
        let (connection, mut events) = test_connection(address, None, TransportMode::Gateway);
        expect_open(&mut events);

        let dropped_at = Instant::now();
        drop(connection);
        let closed_at = block_recv(&mut peer_closed_events);
        assert!(closed_at.duration_since(dropped_at) < DISPOSE_CLOSE_TIMEOUT);
    }

    #[test]
    fn a_disposed_connection_refuses_further_commands() {
        let address = spawn_echo_server();
        let (connection, mut events) = test_connection(address, None, TransportMode::Gateway);
        expect_open(&mut events);

        connection.dispose().unwrap();
        connection.dispose().unwrap();
        assert!(connection.send_text("late".to_owned()).is_err());
        assert!(connection.close(1000, String::new()).is_err());
    }

    #[test]
    fn frames_beyond_the_transport_mode_maximum_are_refused_before_the_queue() {
        let address = spawn_echo_server();
        let (connection, mut events) = test_connection(address, None, TransportMode::Voice);
        expect_open(&mut events);

        let error = connection
            .send_text("a".repeat(VOICE_FRAME_MAX_BYTES + 1))
            .unwrap_err();
        assert_eq!(error.status, Status::InvalidArg);
        assert!(
            connection
                .send_frame(Message::Binary(vec![0u8; VOICE_FRAME_MAX_BYTES + 1].into()))
                .is_err()
        );
        assert!(
            connection
                .send_text("a".repeat(VOICE_FRAME_MAX_BYTES))
                .is_ok()
        );
    }

    #[test]
    fn an_unreachable_endpoint_reports_an_error_and_an_unclean_close() {
        let runtime = transport_runtime().expect("transport runtime");
        let listener = runtime
            .block_on(TcpListener::bind("127.0.0.1:0"))
            .expect("bind test listener");
        let address = listener.local_addr().expect("listener address");
        drop(listener);

        let (_connection, mut events) = test_connection(address, None, TransportMode::Gateway);
        let error = next_event(&mut events);
        assert_eq!(error.kind, EVENT_KIND_ERROR);
        let close = next_event(&mut events);
        assert_eq!(close.kind, EVENT_KIND_CLOSE);
        assert_eq!(close.code, Some(1006));
        assert_eq!(close.was_clean, Some(false));
    }
}
