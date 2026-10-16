// SPDX-License-Identifier: AGPL-3.0-or-later

import {readRawStorageItem, writeRawStorageItem} from '@app/features/platform/state/PrebootMirror';

export const CLIENT_INSTALLATION_ID_STORAGE_KEY = 'ClientInstallationId';

const CLIENT_INSTALLATION_ID_PATTERN = /^[0-9a-f]{32}$/;
const CLIENT_INSTALLATION_ID_BYTE_LENGTH = 16;

function generateClientInstallationId(): string {
	const bytes = new Uint8Array(CLIENT_INSTALLATION_ID_BYTE_LENGTH);
	crypto.getRandomValues(bytes);
	let id = '';
	for (const byte of bytes) {
		id += byte.toString(16).padStart(2, '0');
	}
	return id;
}

class ClientInstallationIdOwner {
	private cachedId: string | null = null;

	get(): string {
		if (this.cachedId !== null) {
			return this.cachedId;
		}
		const stored = readRawStorageItem(CLIENT_INSTALLATION_ID_STORAGE_KEY);
		if (stored !== null && CLIENT_INSTALLATION_ID_PATTERN.test(stored)) {
			this.cachedId = stored;
			return stored;
		}
		const created = generateClientInstallationId();
		writeRawStorageItem(CLIENT_INSTALLATION_ID_STORAGE_KEY, created);
		this.cachedId = created;
		return created;
	}
}

export const ClientInstallationId = new ClientInstallationIdOwner();
