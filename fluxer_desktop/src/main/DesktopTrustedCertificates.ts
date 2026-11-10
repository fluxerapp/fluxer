// SPDX-License-Identifier: AGPL-3.0-or-later

import {X509Certificate} from 'node:crypto';
import {Worker} from 'node:worker_threads';
import {createChildLogger} from '@electron/common/Logger';
import {readWindowsIntermediateStoreRoots} from '@electron/main/GatewaySocketNativeBoundary';

declare const __FLUXER_CERTIFICATE_WORKER_SOURCE__: string;

const logger = createChildLogger('DesktopTrustedCertificates');

interface DesktopCertificates {
	readonly trusted: ReadonlyArray<string>;
	readonly gateway: ReadonlyArray<string>;
	readonly platformAnchors: ReadonlyArray<string>;
}

const EMPTY_CERTIFICATES: DesktopCertificates = {trusted: [], gateway: [], platformAnchors: []};
let certificateStores: Promise<Pick<DesktopCertificates, 'trusted' | 'platformAnchors'>> | null = null;
let certificates: Promise<DesktopCertificates> | null = null;
let certificateWorker: Worker | null = null;
let closing = false;

function readCertificateStores(): Promise<Pick<DesktopCertificates, 'trusted' | 'platformAnchors'>> {
	if (closing) return Promise.resolve(EMPTY_CERTIFICATES);
	certificateStores ??= new Promise((resolve) => {
		let settled = false;
		const finish = (result: Pick<DesktopCertificates, 'trusted' | 'platformAnchors'>): void => {
			if (settled) return;
			settled = true;
			resolve(result);
		};

		let worker: Worker;
		try {
			worker = new Worker(__FLUXER_CERTIFICATE_WORKER_SOURCE__, {eval: true});
		} catch (error) {
			logger.warn('Could not start the certificate store reader, continuing without its certificates', error);
			finish(EMPTY_CERTIFICATES);
			return;
		}
		certificateWorker = worker;
		worker.on(
			'message',
			(
				message: Pick<DesktopCertificates, 'trusted' | 'platformAnchors'> & {
					type: string;
					store?: string;
					error?: string;
				},
			) => {
				if (message.type === 'warning') {
					logger.warn('Could not read a certificate store, continuing without it', {
						store: message.store,
						error: message.error,
					});
				} else if (message.type === 'certificates') {
					finish(message);
				}
			},
		);
		worker.once('error', (error) => {
			logger.warn('The certificate store reader failed, continuing without its certificates', error);
			finish(EMPTY_CERTIFICATES);
		});
		worker.once('exit', () => {
			if (certificateWorker === worker) certificateWorker = null;
			finish(EMPTY_CERTIFICATES);
		});
		worker.unref();
	});
	return certificateStores;
}

function readCertificates(): Promise<DesktopCertificates> {
	if (closing) return Promise.resolve(EMPTY_CERTIFICATES);
	certificates ??= readCertificateStores().then((result) => {
		if (closing) {
			return EMPTY_CERTIFICATES;
		}
		const platformAnchors =
			process.platform === 'win32'
				? readWindowsIntermediateStoreRoots().flatMap((anchor) => {
						try {
							return [new X509Certificate(anchor).toString()];
						} catch {
							return [];
						}
					})
				: result.platformAnchors;
		return {
			trusted: result.trusted,
			gateway: [...new Set([...result.trusted, ...platformAnchors])],
			platformAnchors,
		};
	});
	return certificates;
}

export function prewarmSystemCertificateStore(): void {
	void readCertificateStores();
}

export async function resolveDesktopTrustedCertificates(): Promise<ReadonlyArray<string>> {
	return (await readCertificates()).trusted;
}

export async function resolveDesktopGatewayTrustedCertificates(): Promise<ReadonlyArray<string>> {
	return (await readCertificates()).gateway;
}

export async function resolveDesktopPlatformTrustAnchors(): Promise<ReadonlyArray<string>> {
	return (await readCertificates()).platformAnchors;
}

export async function cleanupDesktopTrustedCertificates(): Promise<void> {
	closing = true;
	await certificateWorker?.terminate();
	certificateWorker = null;
	certificateStores = null;
	certificates = null;
}
