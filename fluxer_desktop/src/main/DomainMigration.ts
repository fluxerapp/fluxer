// SPDX-License-Identifier: AGPL-3.0-or-later

import {getOfficialAppOrigins, setAppOrigin} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import {type IpcMainInvokeEvent, ipcMain} from 'electron';

const logger = createChildLogger('DomainMigration');

const DOMAIN_MIGRATION_SET_APP_ORIGIN_CHANNEL = 'domain-migration:set-app-origin';

function getTopLevelSenderOrigin(event: IpcMainInvokeEvent): string | null {
	const frame = event.senderFrame;
	if (frame == null) {
		return null;
	}
	try {
		if (frame.detached || frame.parent != null) {
			return null;
		}
		return new URL(frame.url).origin;
	} catch {
		return null;
	}
}

function handleSetAppOrigin(event: IpcMainInvokeEvent, origin: unknown): void {
	const officialAppOrigins = getOfficialAppOrigins();
	const senderOrigin = getTopLevelSenderOrigin(event);
	if (senderOrigin === null || !officialAppOrigins.includes(senderOrigin)) {
		throw new Error(`${DOMAIN_MIGRATION_SET_APP_ORIGIN_CHANNEL} is only reachable from an official app document`);
	}
	if (typeof origin !== 'string' || !officialAppOrigins.includes(origin)) {
		throw new Error(`${DOMAIN_MIGRATION_SET_APP_ORIGIN_CHANNEL} received an origin outside the allowlist`);
	}
	if (!setAppOrigin(origin)) {
		throw new Error(`${DOMAIN_MIGRATION_SET_APP_ORIGIN_CHANNEL} could not store the app origin`);
	}
	logger.info('Stored app origin', {origin, senderOrigin});
}

export function registerDomainMigrationHandlers(): void {
	ipcMain.handle(DOMAIN_MIGRATION_SET_APP_ORIGIN_CHANNEL, handleSetAppOrigin);
}
