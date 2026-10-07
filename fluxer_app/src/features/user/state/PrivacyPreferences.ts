// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import Users from '@app/features/user/state/Users';
import {PrivacyPreferencesSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {extractTimestampFromSnowflake} from '@fluxer/snowflake/src/SnowflakeUtils';
import {makeAutoObservable} from 'mobx';

const SHOW_ACTIVE_NOW_DEFAULT = true;
const PREUPLOAD_MESSAGE_ATTACHMENTS_DEFAULT_ON_AFTER = '2026-06-04T16:10:00.000Z';
const PREUPLOAD_MESSAGE_ATTACHMENTS_DEFAULT_ON_AFTER_MS = Date.parse(PREUPLOAD_MESSAGE_ATTACHMENTS_DEFAULT_ON_AFTER);

function getPreuploadMessageAttachmentsDefaultForUserId(userId: string | null | undefined): boolean {
	if (!userId) {
		return false;
	}
	const createdAtMs = extractTimestampFromSnowflake(userId);
	return Number.isFinite(createdAtMs) && createdAtMs >= PREUPLOAD_MESSAGE_ATTACHMENTS_DEFAULT_ON_AFTER_MS;
}

function currentUserIdSafe(): string | null | undefined {
	return (Users as typeof Users | undefined)?.currentUserId;
}

class PrivacyPreferences {
	disableStreamPreviews = false;
	showActiveNow = SHOW_ACTIVE_NOW_DEFAULT;
	preuploadMessageAttachments: boolean | undefined = undefined;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'privacy',
			schema: PrivacyPreferencesSchema,
			persist: ['disableStreamPreviews', 'showActiveNow', 'preuploadMessageAttachments'],
			toMessage: (s) => ({
				disableStreamPreviews: s.disableStreamPreviews,
				showActiveNow: s.showActiveNow === SHOW_ACTIVE_NOW_DEFAULT ? undefined : s.showActiveNow,
				preuploadMessageAttachments:
					s.preuploadMessageAttachments === getPreuploadMessageAttachmentsDefaultForUserId(currentUserIdSafe())
						? undefined
						: s.preuploadMessageAttachments,
			}),
			applyMessage: (s, m) => {
				s.disableStreamPreviews = m.disableStreamPreviews;
				if (m.showActiveNow !== undefined) {
					s.showActiveNow = m.showActiveNow;
				}
				s.preuploadMessageAttachments = m.preuploadMessageAttachments;
			},
		});
	}

	getDisableStreamPreviews(): boolean {
		return this.disableStreamPreviews;
	}

	getShowActiveNow(): boolean {
		return this.showActiveNow;
	}

	getPreuploadMessageAttachments(): boolean {
		return this.preuploadMessageAttachments ?? getPreuploadMessageAttachmentsDefaultForUserId(currentUserIdSafe());
	}

	setDisableStreamPreviews(value: boolean): void {
		this.disableStreamPreviews = value;
	}

	setShowActiveNow(value: boolean): void {
		this.showActiveNow = value;
	}

	setPreuploadMessageAttachments(value: boolean): void {
		const defaultValue = getPreuploadMessageAttachmentsDefaultForUserId(currentUserIdSafe());
		this.preuploadMessageAttachments = value === defaultValue ? undefined : value;
	}
}

export default new PrivacyPreferences();
