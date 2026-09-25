// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {readDomainMigrationDiscovery} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import {isPasskeyMigrationOrigin} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import {openPasskeyUpdateModal} from '@app/features/auth/passkey_migration/PasskeyUpdateModal';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Modal from '@app/features/ui/state/Modal';
import WebAuthnCredentials from '@app/features/user/state/WebAuthnCredentials';
import {PASSKEY_MIGRATION_RP_ID} from '@fluxer/constants/src/PasskeyConstants';
import type {PasskeyMigrationResponse} from '@fluxer/schema/src/domains/auth/PasskeyMigrationSchemas';
import {makeAutoObservable, runInAction, when} from 'mobx';

const logger = new Logger('PasskeyMigration');

export type PendingPasskeyMigration = NonNullable<PasskeyMigrationResponse['pending']>;

class PasskeyMigration {
	pending: PendingPasskeyMigration | null = null;
	private checkedUserId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	async check(): Promise<void> {
		try {
			const response = await http.get<PasskeyMigrationResponse>(Endpoints.USER_MFA_WEBAUTHN_MIGRATION);
			const pending = response.body.pending;
			runInAction(() => {
				this.pending = pending;
			});
			if (pending !== null) {
				openPasskeyUpdateModal(pending);
			}
		} catch (error) {
			logger.warn('Failed to check for a passkey to update', error);
		}
	}

	clear(): void {
		this.pending = null;
	}

	checkAfterSudo(sudoModalKey: string): void {
		const requester = Modal.modals.findLast((entry) => entry.key !== sudoModalKey && !entry.isBackground);
		if (requester === undefined) {
			void this.check();
			return;
		}
		when(
			() => !Modal.hasModal(requester.key),
			() => void this.check(),
		);
	}

	handleGatewayReady(userId: string): void {
		if (
			this.checkedUserId === userId ||
			!isPasskeyMigrationOrigin() ||
			readDomainMigrationDiscovery()?.enabled !== true
		) {
			return;
		}
		this.checkedUserId = userId;
		if (WebAuthnCredentials.credentials.some((credential) => credential.rp_id !== PASSKEY_MIGRATION_RP_ID)) {
			void this.check();
		}
	}
}

export default new PasskeyMigration();
