// SPDX-License-Identifier: AGPL-3.0-or-later

import tls from 'node:tls';
import {createChildLogger} from '@electron/common/Logger';

const logger = createChildLogger('DesktopTrustedCertificates');

export type DesktopCertificateStore = 'default' | 'system';
export type DesktopCertificateSource = (store: DesktopCertificateStore) => ReadonlyArray<string>;

function readStore(source: DesktopCertificateSource, store: DesktopCertificateStore): ReadonlyArray<string> {
	try {
		return source(store);
	} catch (error) {
		logger.warn('Could not read a certificate store, continuing without it', {store, error});
		return [];
	}
}

export function resolveDesktopTrustedCertificates(
	source: DesktopCertificateSource = (store) => tls.getCACertificates(store),
): ReadonlyArray<string> {
	const merged = new Set<string>();
	for (const certificate of readStore(source, 'default')) merged.add(certificate);
	for (const certificate of readStore(source, 'system')) merged.add(certificate);
	return [...merged];
}
