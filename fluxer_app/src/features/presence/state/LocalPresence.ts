// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountPresenceIntent} from '@app/features/auth/state/AccountStorage';
import type {ActivityPayload} from '@app/features/gateway/types/GatewayPresenceTypes';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import Idle from '@app/features/ui/state/Idle';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {CustomStatus, GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import {customStatusToKey, normalizeCustomStatus, toGatewayCustomStatus} from '@app/features/user/state/CustomStatus';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {normalizeStatus, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {makeAutoObservable, reaction} from 'mobx';

type Presence = Readonly<{
	status: StatusType;
	since: number;
	afk: boolean;
	mobile: boolean;
	custom_status: GatewayCustomStatusPayload | null;
	activities?: readonly ActivityPayload[] | null;
}>;

export const ACCOUNT_PRESENCE_INTENT_MAX_AGE_MS = 60 * 1000;

interface LocalPresenceUserSettings {
	status: StatusType;
	isHydrated(): boolean;
	markSessionChanging(): void;
	getAfkTimeout(): number;
	getCustomStatus(): CustomStatus | null;
	getStatusResetsAt(): string | null;
	getStatusResetsTo(): string | null;
}

let userSettings: LocalPresenceUserSettings | null = null;

export function setLocalPresenceUserSettings(settings: LocalPresenceUserSettings): void {
	userSettings = settings;
}

class LocalPresence {
	status: StatusType = StatusTypes.ONLINE;
	since: number = 0;
	afk: boolean = false;
	mobile: boolean = false;
	customStatus: CustomStatus | null = null;
	activities: readonly ActivityPayload[] | null = null;
	private restoredIntent: AccountPresenceIntent | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		deferUntilModulesLoaded(() => {
			reaction(
				() => MobileLayout.isMobileLayout(),
				() => this.updatePresence(),
			);
			try {
				const electronApi = getElectronAPI();
				if (electronApi?.onRpcActivityUpdate) {
					electronApi.onRpcActivityUpdate((activity: unknown) => {
						this.setActivity(activity as ActivityPayload | null | readonly ActivityPayload[]);
					});
				}
			} catch {
				// Non-electron environment fallback
			}
		});
	}

	updatePresence(): void {
		const isMobile = MobileLayout.isMobileLayout();
		const settings = userSettings;
		if (!settings?.isHydrated()) {
			if (this.restoredIntent) {
				this.applyRestoredIntent(isMobile, settings);
				return;
			}
			this.status = StatusTypes.INVISIBLE;
			this.since = 0;
			this.afk = false;
			this.mobile = isMobile;
			this.customStatus = null;
			return;
		}
		this.restoredIntent = null;
		const userStatus = settings.status;
		const idleSince = Idle.getIdleSince();
		const afk = this.computeAfk(idleSince, isMobile, settings);
		const effectiveStatus = userStatus === StatusTypes.ONLINE && idleSince > 0 ? StatusTypes.IDLE : userStatus;
		const normalizedCustomStatus = normalizeCustomStatus(settings.getCustomStatus());
		this.customStatus = normalizedCustomStatus ? {...normalizedCustomStatus} : null;
		this.status = effectiveStatus;
		this.since = idleSince;
		this.afk = afk;
		this.mobile = isMobile;
	}

	setActivity(activity: ActivityPayload | null | undefined | readonly ActivityPayload[]): void {
		if (!activity) {
			this.activities = null;
		} else if (Array.isArray(activity)) {
			this.activities = activity.length > 0 ? activity : null;
		} else {
			this.activities = [activity as ActivityPayload];
		}
		this.updatePresence();
	}

	clearActivity(): void {
		this.setActivity(null);
	}

	getActivities(): readonly ActivityPayload[] | null {
		return this.activities;
	}

	getStatus(): StatusType {
		return this.status;
	}

	getPresence(): Presence {
		return {
			status: this.status,
			since: this.since,
			afk: this.afk,
			mobile: this.mobile,
			custom_status: toGatewayCustomStatus(this.customStatus),
			activities: this.activities,
		};
	}

	getGatewayPresence(): Presence | null {
		if (!userSettings?.isHydrated() && !this.restoredIntent) {
			return null;
		}
		return this.getPresence();
	}

	handleSessionChanging(options: {clearRestoredIntent?: boolean; clearActivities?: boolean} = {}): void {
		if (options.clearRestoredIntent) {
			this.restoredIntent = null;
		}
		if (options.clearActivities) {
			this.activities = null;
		}
		userSettings?.markSessionChanging();
		this.updatePresence();
	}

	captureIntent(): AccountPresenceIntent | null {
		const settings = userSettings;
		if (!settings?.isHydrated()) {
			return null;
		}
		const normalizedCustomStatus = normalizeCustomStatus(settings.getCustomStatus());
		return {
			status: settings.status,
			statusResetsAt: settings.getStatusResetsAt(),
			statusResetsTo: this.normalizeResetStatus(settings.getStatusResetsTo()),
			customStatus: normalizedCustomStatus ? {...normalizedCustomStatus} : null,
			capturedAt: Date.now(),
		};
	}

	restoreIntent(intent: AccountPresenceIntent | null | undefined): void {
		this.restoredIntent = intent && this.isIntentFresh(intent) ? this.normalizeIntent(intent) : null;
		this.updatePresence();
	}

	get presenceKey(): string {
		const hydrated = userSettings?.isHydrated() ? '1' : '0';
		const afk = this.afk ? '1' : '0';
		const mobile = this.mobile ? '1' : '0';
		const actKey = this.activities && this.activities.length > 0 ? JSON.stringify(this.activities) : 'null';
		return `hydrated:${hydrated}|${this.status}|${customStatusToKey(this.customStatus)}|afk:${afk}|mobile:${mobile}|act:${actKey}`;
	}

	private computeAfk(idleSince: number, isMobile: boolean, settings: LocalPresenceUserSettings | null): boolean {
		if (isMobile || idleSince <= 0) return false;
		const afkTimeout = settings?.getAfkTimeout() ?? 600;
		return Date.now() - idleSince > afkTimeout * 1000;
	}

	private applyRestoredIntent(isMobile: boolean, settings: LocalPresenceUserSettings | null): void {
		if (!this.restoredIntent) {
			return;
		}
		const idleSince = Idle.getIdleSince();
		const userStatus = this.restoredIntent.status;
		const effectiveStatus = userStatus === StatusTypes.ONLINE && idleSince > 0 ? StatusTypes.IDLE : userStatus;
		this.customStatus = this.restoredIntent.customStatus ? {...this.restoredIntent.customStatus} : null;
		this.status = effectiveStatus;
		this.since = idleSince;
		this.afk = this.computeAfk(idleSince, isMobile, settings);
		this.mobile = isMobile;
	}

	private normalizeIntent(intent: AccountPresenceIntent): AccountPresenceIntent {
		const statusResetsAt = intent.statusResetsAt ?? null;
		const statusResetsTo = this.normalizeResetStatus(intent.statusResetsTo);
		const expiresAt = statusResetsAt ? Date.parse(statusResetsAt) : null;
		const isExpired = expiresAt !== null && Number.isFinite(expiresAt) && expiresAt <= Date.now();
		const normalizedCustomStatus = normalizeCustomStatus(intent.customStatus);
		return {
			status: isExpired ? (statusResetsTo ?? StatusTypes.ONLINE) : normalizeStatus(intent.status),
			statusResetsAt: isExpired ? null : statusResetsAt,
			statusResetsTo: isExpired ? null : statusResetsTo,
			customStatus: normalizedCustomStatus ? {...normalizedCustomStatus} : null,
			capturedAt: intent.capturedAt,
		};
	}

	private isIntentFresh(intent: AccountPresenceIntent): boolean {
		const ageMs = Date.now() - intent.capturedAt;
		return Number.isFinite(intent.capturedAt) && ageMs >= 0 && ageMs <= ACCOUNT_PRESENCE_INTENT_MAX_AGE_MS;
	}

	private normalizeResetStatus(status: string | null | undefined): StatusType | null {
		if (!status) {
			return null;
		}
		return normalizeStatus(status);
	}
}

export default new LocalPresence();
