// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountEngine} from '@electron/engine/AccountEngine';
import {systemClock} from '@electron/engine/EngineClock';
import {resolveGatewayTransport} from '@electron/engine/GatewayTransport';
import {HostChannel} from '@electron/engine/HostChannel';
import {ACCOUNT_ENGINE_SNAPSHOT_SCHEMA_EPOCH, createAccountEngineCore} from '@fluxer/account-engine-core';

function silenceExperimentalWarnings(): void {
	process.removeAllListeners('warning');
	process.on('warning', (warning) => {
		if (warning.name !== 'ExperimentalWarning') {
			console.warn(warning);
		}
	});
}

async function runAccountEngine(): Promise<void> {
	silenceExperimentalWarnings();
	const host = await HostChannel.connect();
	let engine: AccountEngine | null = null;
	host.onMessage((message, port) => {
		if (engine === null) {
			if (message.type !== 'init') {
				return;
			}
			const transport = resolveGatewayTransport({
				preferWebSocket: message.preferWebSocket,
				trustedCertificates: message.trustedCertificates,
			});
			engine = new AccountEngine({
				send: (outbound) => host.send(outbound),
				clock: systemClock,
				transport: transport.factory,
				transportKind: transport.kind,
				transportError: transport.loadError,
				createCore: createAccountEngineCore,
				snapshotSchemaEpoch: ACCOUNT_ENGINE_SNAPSHOT_SCHEMA_EPOCH,
				exit: () => setImmediate(() => process.exit(0)),
				memoryUsage: () => {
					const usage = process.memoryUsage();
					return {rss: usage.rss, heapUsed: usage.heapUsed};
				},
			});
		}
		try {
			engine.handleHostMessage(message, port);
		} catch (error) {
			host.send({
				type: 'log',
				level: 'error',
				message: 'Account engine failed to handle a host message',
				detail: error instanceof Error ? (error.stack ?? error.message) : String(error),
			});
		}
	});
}

process.on('uncaughtException', (error) => {
	console.error('[AccountEngine] uncaught exception', error);
	process.exit(70);
});

process.on('unhandledRejection', (reason) => {
	console.error('[AccountEngine] unhandled rejection', reason);
});

process.on('disconnect', () => process.exit(0));

void runAccountEngine();
