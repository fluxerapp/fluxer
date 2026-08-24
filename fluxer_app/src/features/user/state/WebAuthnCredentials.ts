// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('WebAuthnCredentials');

export interface WebAuthnCredential {
	id: string;
	name: string;
	created_at: string;
	last_used_at: string | null;
}

class WebAuthnCredentials {
	credentialsArray: Array<WebAuthnCredential> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setCredentials(credentials: ReadonlyArray<WebAuthnCredential>): void {
		this.credentialsArray = [...credentials];
		logger.debug(`Set WebAuthn credentials: ${credentials.length}`);
	}

	handleGatewayReady(credentials: ReadonlyArray<WebAuthnCredential> | undefined): void {
		this.setCredentials(credentials ?? []);
	}

	get credentials(): ReadonlyArray<WebAuthnCredential> {
		return this.credentialsArray;
	}
}

export default new WebAuthnCredentials();
