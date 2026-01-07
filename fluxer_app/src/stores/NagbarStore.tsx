/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';

export interface NagbarSettings {
	iosInstallDismissed: boolean;
	pwaInstallDismissed: boolean;
	pushNotificationDismissed: boolean;
	desktopNotificationDismissed: boolean;
	premiumGracePeriodDismissed: boolean;
	premiumExpiredDismissed: boolean;
	premiumOnboardingDismissed: boolean;
	giftInventoryDismissed: boolean;
	desktopDownloadDismissed: boolean;
	mobileDownloadDismissed: boolean;
	pendingBulkDeletionDismissed: Record<string, boolean>;
	invitesDisabledDismissed: Record<string, boolean>;
	guildMembershipCtaDismissed: boolean;
	claimAccountModalShownThisSession: boolean;
	forceOffline: boolean;
	forceEmailVerification: boolean;
	forceIOSInstall: boolean;
	forcePWAInstall: boolean;
	forcePushNotification: boolean;
	forceUnclaimedAccount: boolean;
	forceDesktopNotification: boolean;
	forceInvitesDisabled: boolean;
	forcePremiumGracePeriod: boolean;
	forcePremiumExpired: boolean;
	forcePremiumOnboarding: boolean;
	forceGiftInventory: boolean;
	forceUpdateAvailable: boolean;
	forceDesktopDownload: boolean;
	forceMobileDownload: boolean;
	forceGuildMembershipCta: boolean;
	forceHideOffline: boolean;
	forceHideEmailVerification: boolean;
	forceHideIOSInstall: boolean;
	forceHidePWAInstall: boolean;
	forceHidePushNotification: boolean;
	forceHideUnclaimedAccount: boolean;
	forceHideDesktopNotification: boolean;
	forceHideInvitesDisabled: boolean;
	forceHidePremiumGracePeriod: boolean;
	forceHidePremiumExpired: boolean;
	forceHidePremiumOnboarding: boolean;
	forceHideGiftInventory: boolean;
	forceHideUpdateAvailable: boolean;
	forceHideDesktopDownload: boolean;
	forceHideMobileDownload: boolean;
	forceHideGuildMembershipCta: boolean;
}

export type NagbarToggleKey = Exclude<
	keyof NagbarSettings,
	'invitesDisabledDismissed' | 'claimAccountModalShownThisSession' | 'pendingBulkDeletionDismissed'
>;

export class NagbarStore implements NagbarSettings {
	iosInstallDismissed = false;
	pwaInstallDismissed = false;
	pushNotificationDismissed = false;
	desktopNotificationDismissed = false;
	premiumGracePeriodDismissed = false;
	premiumExpiredDismissed = false;
	premiumOnboardingDismissed = false;
	giftInventoryDismissed = false;
	desktopDownloadDismissed = false;
	mobileDownloadDismissed = false;
	pendingBulkDeletionDismissed: Record<string, boolean> = {};
	invitesDisabledDismissed: Record<string, boolean> = {};
	guildMembershipCtaDismissed = false;
	claimAccountModalShownThisSession = false;
	forceOffline = false;
	forceEmailVerification = false;
	forceIOSInstall = false;
	forcePWAInstall = false;
	forcePushNotification = false;
	forceUnclaimedAccount = false;
	forceDesktopNotification = false;
	forceInvitesDisabled = false;
	forcePremiumGracePeriod = false;
	forcePremiumExpired = false;
	forcePremiumOnboarding = false;
	forceGiftInventory = false;
	forceUpdateAvailable = false;
	forceDesktopDownload = false;
	forceMobileDownload = false;
	forceGuildMembershipCta = false;

	forceHideOffline = false;
	forceHideEmailVerification = false;
	forceHideIOSInstall = false;
	forceHidePWAInstall = false;
	forceHidePushNotification = false;
	forceHideUnclaimedAccount = false;
	forceHideDesktopNotification = false;
	forceHideInvitesDisabled = false;
	forceHidePremiumGracePeriod = false;
	forceHidePremiumExpired = false;
	forceHidePremiumOnboarding = false;
	forceHideGiftInventory = false;
	forceHideUpdateAvailable = false;
	forceHideDesktopDownload = false;
	forceHideMobileDownload = false;
	forceHideGuildMembershipCta = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'NagbarStore', [
			'iosInstallDismissed',
			'pwaInstallDismissed',
			'pushNotificationDismissed',
			'desktopNotificationDismissed',
			'premiumGracePeriodDismissed',
			'premiumExpiredDismissed',
			'premiumOnboardingDismissed',
			'giftInventoryDismissed',
			'desktopDownloadDismissed',
			'mobileDownloadDismissed',
			'pendingBulkDeletionDismissed',
			'invitesDisabledDismissed',
			'guildMembershipCtaDismissed',
		]);
	}

	getIosInstallDismissed(): boolean {
		return this.iosInstallDismissed;
	}

	getPwaInstallDismissed(): boolean {
		return this.pwaInstallDismissed;
	}

	getPushNotificationDismissed(): boolean {
		return this.pushNotificationDismissed;
	}

	getForceOffline(): boolean {
		return this.forceOffline;
	}

	getForceEmailVerification(): boolean {
		return this.forceEmailVerification;
	}

	getForceIOSInstall(): boolean {
		return this.forceIOSInstall;
	}

	getForcePWAInstall(): boolean {
		return this.forcePWAInstall;
	}

	getForcePushNotification(): boolean {
		return this.forcePushNotification;
	}

	getForceUnclaimedAccount(): boolean {
		return this.forceUnclaimedAccount;
	}

	getInvitesDisabledDismissed(guildId: string): boolean {
		return this.invitesDisabledDismissed[guildId] ?? false;
	}

	getForceInvitesDisabled(): boolean {
		return this.forceInvitesDisabled;
	}

	getForceHideOffline(): boolean {
		return this.forceHideOffline;
	}

	getForceHideEmailVerification(): boolean {
		return this.forceHideEmailVerification;
	}

	getForceHideIOSInstall(): boolean {
		return this.forceHideIOSInstall;
	}

	getForceHidePWAInstall(): boolean {
		return this.forceHidePWAInstall;
	}

	getForceHidePushNotification(): boolean {
		return this.forceHidePushNotification;
	}

	getForceHideUnclaimedAccount(): boolean {
		return this.forceHideUnclaimedAccount;
	}

	getForceHideDesktopNotification(): boolean {
		return this.forceHideDesktopNotification;
	}

	getForceHideInvitesDisabled(): boolean {
		return this.forceHideInvitesDisabled;
	}

	getForceHidePremiumGracePeriod(): boolean {
		return this.forceHidePremiumGracePeriod;
	}

	getForceHidePremiumExpired(): boolean {
		return this.forceHidePremiumExpired;
	}

	getForceHidePremiumOnboarding(): boolean {
		return this.forceHidePremiumOnboarding;
	}

	getForceHideGiftInventory(): boolean {
		return this.forceHideGiftInventory;
	}

	getForceHideUpdateAvailable(): boolean {
		return this.forceHideUpdateAvailable;
	}

	getForceGuildMembershipCta(): boolean {
		return this.forceGuildMembershipCta;
	}

	getForceHideGuildMembershipCta(): boolean {
		return this.forceHideGuildMembershipCta;
	}

	hasPendingBulkDeletionDismissed(scheduleKey: string | null): boolean {
		if (!scheduleKey) {
			return false;
		}

		return Boolean(this.pendingBulkDeletionDismissed[scheduleKey]);
	}

	markClaimAccountModalShown(): void {
		this.claimAccountModalShownThisSession = true;
	}

	resetClaimAccountModalShown(): void {
		this.claimAccountModalShownThisSession = false;
	}

	dismiss(nagbarType: NagbarToggleKey): void {
		this[nagbarType] = true;
	}

	dismissPendingBulkDeletion(scheduleKey: string): void {
		this.pendingBulkDeletionDismissed = {
			...this.pendingBulkDeletionDismissed,
			[scheduleKey]: true,
		};
	}

	dismissInvitesDisabled(guildId: string): void {
		this.invitesDisabledDismissed = {
			...this.invitesDisabledDismissed,
			[guildId]: true,
		};
	}

	clearPendingBulkDeletionDismissed(scheduleKey: string): void {
		const {[scheduleKey]: _, ...rest} = this.pendingBulkDeletionDismissed;
		this.pendingBulkDeletionDismissed = rest;
	}

	reset(nagbarType: NagbarToggleKey): void {
		this[nagbarType] = false;
	}

	setFlag(key: NagbarToggleKey, value: boolean): void {
		this[key] = value;
	}

	resetInvitesDisabled(guildId: string): void {
		const {[guildId]: _, ...rest} = this.invitesDisabledDismissed;
		this.invitesDisabledDismissed = rest;
	}

	resetAll(): void {
		this.iosInstallDismissed = false;
		this.pwaInstallDismissed = false;
		this.pushNotificationDismissed = false;
		this.desktopNotificationDismissed = false;
		this.premiumGracePeriodDismissed = false;
		this.premiumExpiredDismissed = false;
		this.premiumOnboardingDismissed = false;
		this.giftInventoryDismissed = false;
		this.desktopDownloadDismissed = false;
		this.mobileDownloadDismissed = false;
		this.pendingBulkDeletionDismissed = {};
		this.invitesDisabledDismissed = {};
		this.guildMembershipCtaDismissed = false;
		this.claimAccountModalShownThisSession = false;

		this.forceOffline = false;
		this.forceEmailVerification = false;
		this.forceIOSInstall = false;
		this.forcePWAInstall = false;
		this.forcePushNotification = false;
		this.forceUnclaimedAccount = false;
		this.forceDesktopNotification = false;
		this.forceInvitesDisabled = false;
		this.forcePremiumGracePeriod = false;
		this.forcePremiumExpired = false;
		this.forcePremiumOnboarding = false;
		this.forceGiftInventory = false;
		this.forceUpdateAvailable = false;
		this.forceDesktopDownload = false;
		this.forceMobileDownload = false;
		this.forceGuildMembershipCta = false;

		this.forceHideOffline = false;
		this.forceHideEmailVerification = false;
		this.forceHideIOSInstall = false;
		this.forceHidePWAInstall = false;
		this.forceHidePushNotification = false;
		this.forceHideUnclaimedAccount = false;
		this.forceHideDesktopNotification = false;
		this.forceHideInvitesDisabled = false;
		this.forceHidePremiumGracePeriod = false;
		this.forceHidePremiumExpired = false;
		this.forceHidePremiumOnboarding = false;
		this.forceHideGiftInventory = false;
		this.forceHideUpdateAvailable = false;
		this.forceHideDesktopDownload = false;
		this.forceHideMobileDownload = false;
		this.forceHideGuildMembershipCta = false;
	}

	handleGuildUpdate(action: {
		guild: {
			id: string;
			features?: ReadonlyArray<string>;
			properties?: {features: ReadonlyArray<string>};
		};
	}): void {
		const guildId = action.guild.id;
		const features: ReadonlyArray<string> = action.guild.features ?? action.guild.properties?.features ?? [];
		const hasInvitesDisabled = features.includes('INVITES_DISABLED');

		if (!hasInvitesDisabled && this.invitesDisabledDismissed[guildId]) {
			const {[guildId]: _, ...rest} = this.invitesDisabledDismissed;
			this.invitesDisabledDismissed = rest;
		}
	}
}

export default new NagbarStore();
