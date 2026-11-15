// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import apiAttachmentMedia from '@app/features/messaging/state/ApiAttachmentMedia';
import {isApiProxiedAttachmentsEnabled} from '@app/features/messaging/utils/ApiAttachmentUrl';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {reaction} from 'mobx';

const logger = new Logger('ApiAttachmentDesktopAuthorization');

interface DesktopGrant {
	apiEndpoint: string | null;
	token: string | null;
}

let generation = 0;
let grantedEndpoint: string | null = null;

type SetAuthorization = (apiEndpoint: string, token: string | null) => Promise<boolean>;

async function pushGrant(setAuthorization: SetAuthorization, {apiEndpoint, token}: DesktopGrant): Promise<void> {
	generation += 1;
	const current = generation;
	const endpoint = apiEndpoint ?? grantedEndpoint;
	const nextToken = apiEndpoint === null ? null : token;
	if (nextToken === null) apiAttachmentMedia.setHeaderMode(false);
	if (endpoint === null) return;
	grantedEndpoint = nextToken === null ? null : endpoint;
	let granted = false;
	try {
		granted = (await setAuthorization(endpoint, nextToken)) === true;
	} catch (error) {
		logger.warn('Could not hand the session to the desktop app for attachments', error);
	}
	if (current === generation) apiAttachmentMedia.setHeaderMode(granted && nextToken !== null);
}

export function startApiAttachmentDesktopAuthorization(): void {
	const setAuthorization = getElectronAPI()?.setApiAttachmentAuthorization;
	if (setAuthorization === undefined) return;
	reaction(
		(): DesktopGrant => {
			const enabled = isApiProxiedAttachmentsEnabled();
			return {
				apiEndpoint: enabled ? (RuntimeConfig.getSnapshotOrNull()?.apiEndpoint ?? null) : null,
				token: enabled ? SessionManager.token : null,
			};
		},
		(grant) => void pushGrant(setAuthorization, grant),
		{fireImmediately: true, equals: (a, b) => a.apiEndpoint === b.apiEndpoint && a.token === b.token},
	);
}
