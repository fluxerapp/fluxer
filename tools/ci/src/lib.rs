// SPDX-License-Identifier: AGPL-3.0-or-later

mod app_dev_server;
mod app_proxy;
mod app_wasm;
mod calver;
mod ci_workflow;
mod common;
mod desktop;
mod desktop_native;
mod functions;
mod gateway;
mod release;
mod schema;
mod static_bucket;

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(name = "fluxer-ci")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
#[allow(clippy::large_enum_variant)]
enum Command {
    AppDevServer(app_dev_server::AppDevServerArgs),
    BuildAppWasm(app_wasm::BuildAppWasmArgs),
    BuildAppProxy(app_proxy::BuildAppProxyArgs),
    BuildMarkdownParserWasm(app_wasm::BuildMarkdownParserWasmArgs),
    BuildDesktop(desktop::BuildDesktopArgs),
    BuildDesktopNativeAddon(desktop_native::BuildDesktopNativeAddonArgs),
    BuildGatewayNifs(gateway::BuildGatewayNifsArgs),
    Ci(ci_workflow::CiArgs),
    CiScripts(ci_workflow::CiScriptsArgs),
    CleanSchemaGeneratedFiles(schema::CleanSchemaGeneratedFilesArgs),
    Gateway(gateway::GatewayArgs),
    RepairStaticAssetMetadata(static_bucket::RepairStaticAssetMetadataArgs),
    Release(release::ReleaseArgs),
    ResolveCalver(calver::ResolveCalverArgs),
    SyncStaticBucket(static_bucket::SyncStaticBucketArgs),
    TestWebrtcSenderRust(desktop_native::TestWebrtcSenderRustArgs),
}

pub async fn run() -> Result<()> {
    match Cli::parse().command {
        Command::AppDevServer(args) => app_dev_server::run(args).await,
        Command::BuildAppWasm(args) => app_wasm::run_build_app_wasm(args),
        Command::BuildAppProxy(args) => app_proxy::run(args).await,
        Command::BuildMarkdownParserWasm(args) => app_wasm::run_build_markdown_parser_wasm(args),
        Command::BuildDesktop(args) => desktop::run(args).await,
        Command::BuildDesktopNativeAddon(args) => {
            desktop_native::run_build_desktop_native_addon(args)
        }
        Command::BuildGatewayNifs(args) => gateway::run_build_gateway_nifs(args),
        Command::Ci(args) => ci_workflow::run_ci(args).await,
        Command::CiScripts(args) => ci_workflow::run_ci_scripts(args).await,
        Command::CleanSchemaGeneratedFiles(args) => schema::run_clean_generated_files(args),
        Command::Gateway(args) => gateway::run_gateway(args),
        Command::RepairStaticAssetMetadata(args) => {
            static_bucket::repair_asset_metadata(args).await
        }
        Command::Release(args) => release::run(args).await,
        Command::ResolveCalver(args) => calver::run(args),
        Command::SyncStaticBucket(args) => static_bucket::run(args).await,
        Command::TestWebrtcSenderRust(args) => desktop_native::run_test_webrtc_sender_rust(args),
    }
}
