// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {NagbarDismissalsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

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
	pendingBulkDeletionDismissed: Record<string, boolean>;
	invitesDisabledDismissed: Record<string, boolean>;
	guildMfaRequirementDismissed: Record<string, boolean>;
	guildMembershipCtaDismissed: boolean;
	visionaryMfaDismissed: boolean;
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
	forceDesktopDownload: boolean;
	forceGuildMembershipCta: boolean;
	forceVisionaryMfa: boolean;
	forceTermsAcceptance: boolean;
	forceCorruptedInstallation: boolean;
	forceScheduledMaintenance: boolean;
	forceVoiceSessionRestore: boolean;
	forceGuildMfaRequirement: boolean;
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
	forceHideDesktopDownload: boolean;
	forceHideGuildMembershipCta: boolean;
	forceHideVisionaryMfa: boolean;
	forceHideTermsAcceptance: boolean;
	forceHideCorruptedInstallation: boolean;
	forceHideScheduledMaintenance: boolean;
	forceHideVoiceSessionRestore: boolean;
	forceHideGuildMfaRequirement: boolean;
}

export type NagbarToggleKey = Exclude<
	keyof NagbarSettings,
	| 'guildMfaRequirementDismissed'
	| 'invitesDisabledDismissed'
	| 'claimAccountModalShownThisSession'
	| 'pendingBulkDeletionDismissed'
>;

export class Nagbar implements NagbarSettings {
	iosInstallDismissed = false;
	pwaInstallDismissed = false;
	pushNotificationDismissed = false;
	desktopNotificationDismissed = false;
	premiumGracePeriodDismissed = false;
	premiumExpiredDismissed = false;
	premiumOnboardingDismissed = false;
	giftInventoryDismissed = false;
	desktopDownloadDismissed = false;
	pendingBulkDeletionDismissed: Record<string, boolean> = {};
	invitesDisabledDismissed: Record<string, boolean> = {};
	guildMfaRequirementDismissed: Record<string, boolean> = {};
	guildMembershipCtaDismissed = false;
	visionaryMfaDismissed = false;
	scheduledMaintenanceDismissalVersion = 0;
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
	forceDesktopDownload = false;
	forceGuildMembershipCta = false;
	forceVisionaryMfa = false;
	forceTermsAcceptance = false;
	forceCorruptedInstallation = false;
	forceScheduledMaintenance = false;
	forceVoiceSessionRestore = false;
	forceGuildMfaRequirement = false;
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
	forceHideDesktopDownload = false;
	forceHideGuildMembershipCta = false;
	forceHideVisionaryMfa = false;
	forceHideTermsAcceptance = false;
	forceHideCorruptedInstallation = false;
	forceHideScheduledMaintenance = false;
	forceHideVoiceSessionRestore = false;
	forceHideGuildMfaRequirement = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'nagbars',
			schema: NagbarDismissalsSchema,
			persist: [
				'iosInstallDismissed',
				'pwaInstallDismissed',
				'pushNotificationDismissed',
				'desktopNotificationDismissed',
				'premiumGracePeriodDismissed',
				'premiumExpiredDismissed',
				'premiumOnboardingDismissed',
				'giftInventoryDismissed',
				'desktopDownloadDismissed',
				'pendingBulkDeletionDismissed',
				'invitesDisabledDismissed',
				'guildMfaRequirementDismissed',
				'guildMembershipCtaDismissed',
				'visionaryMfaDismissed',
			],
			toMessage: (s) => ({
				iosInstall: s.iosInstallDismissed,
				pwaInstall: s.pwaInstallDismissed,
				pushNotification: s.pushNotificationDismissed,
				desktopNotification: s.desktopNotificationDismissed,
				premiumGracePeriod: s.premiumGracePeriodDismissed,
				premiumExpired: s.premiumExpiredDismissed,
				premiumOnboarding: s.premiumOnboardingDismissed,
				giftInventory: s.giftInventoryDismissed,
				desktopDownload: s.desktopDownloadDismissed,
				guildMembershipCta: s.guildMembershipCtaDismissed,
				visionaryMfa: s.visionaryMfaDismissed,
				pendingBulkDeletion: {...s.pendingBulkDeletionDismissed},
				invitesDisabled: {...s.invitesDisabledDismissed},
				guildMfaRequirement: {...s.guildMfaRequirementDismissed},
			}),
			applyMessage: (s, m) => {
				s.iosInstallDismissed = m.iosInstall;
				s.pwaInstallDismissed = m.pwaInstall;
				s.pushNotificationDismissed = m.pushNotification;
				s.desktopNotificationDismissed = m.desktopNotification;
				s.premiumGracePeriodDismissed = m.premiumGracePeriod;
				s.premiumExpiredDismissed = m.premiumExpired;
				s.premiumOnboardingDismissed = m.premiumOnboarding;
				s.giftInventoryDismissed = m.giftInventory;
				s.desktopDownloadDismissed = m.desktopDownload;
				s.guildMembershipCtaDismissed = m.guildMembershipCta;
				s.visionaryMfaDismissed = m.visionaryMfa;
				s.pendingBulkDeletionDismissed = {...m.pendingBulkDeletion};
				s.invitesDisabledDismissed = {...m.invitesDisabled};
				s.guildMfaRequirementDismissed = {...m.guildMfaRequirement};
			},
		});
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

	getGuildMfaRequirementDismissed(guildId: string): boolean {
		return this.guildMfaRequirementDismissed[guildId] ?? false;
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

	getForceGuildMembershipCta(): boolean {
		return this.forceGuildMembershipCta;
	}

	getForceHideGuildMembershipCta(): boolean {
		return this.forceHideGuildMembershipCta;
	}

	bumpScheduledMaintenanceDismissed(): void {
		this.scheduledMaintenanceDismissalVersion++;
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

	dismissInvitesDisabled(guildId: string): void {
		this.invitesDisabledDismissed = {
			...this.invitesDisabledDismissed,
			[guildId]: true,
		};
	}

	dismissGuildMfaRequirement(guildId: string): void {
		this.guildMfaRequirementDismissed = {
			...this.guildMfaRequirementDismissed,
			[guildId]: true,
		};
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

	resetGuildMfaRequirement(guildId: string): void {
		const {[guildId]: _, ...rest} = this.guildMfaRequirementDismissed;
		this.guildMfaRequirementDismissed = rest;
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
		this.pendingBulkDeletionDismissed = {};
		this.invitesDisabledDismissed = {};
		this.guildMfaRequirementDismissed = {};
		this.guildMembershipCtaDismissed = false;
		this.visionaryMfaDismissed = false;
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
		this.forceDesktopDownload = false;
		this.forceGuildMembershipCta = false;
		this.forceVisionaryMfa = false;
		this.forceTermsAcceptance = false;
		this.forceCorruptedInstallation = false;
		this.forceScheduledMaintenance = false;
		this.forceVoiceSessionRestore = false;
		this.forceGuildMfaRequirement = false;
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
		this.forceHideDesktopDownload = false;
		this.forceHideGuildMembershipCta = false;
		this.forceHideVisionaryMfa = false;
		this.forceHideTermsAcceptance = false;
		this.forceHideCorruptedInstallation = false;
		this.forceHideScheduledMaintenance = false;
		this.forceHideVoiceSessionRestore = false;
		this.forceHideGuildMfaRequirement = false;
	}

	handleGuildUpdate(action: {
		guild: {
			id: string;
			features?: ReadonlyArray<string>;
			properties?: {
				features: ReadonlyArray<string>;
				mfa_level?: number;
			};
			mfa_level?: number;
		};
	}): void {
		const guildId = action.guild.id;
		const features = action.guild.features ?? action.guild.properties?.features;
		if (features != null && !features.includes('INVITES_DISABLED') && this.invitesDisabledDismissed[guildId]) {
			const {[guildId]: _, ...rest} = this.invitesDisabledDismissed;
			this.invitesDisabledDismissed = rest;
		}
		const mfaLevel = action.guild.mfa_level ?? action.guild.properties?.mfa_level;
		if (mfaLevel === 0 && this.guildMfaRequirementDismissed[guildId]) {
			const {[guildId]: _, ...rest} = this.guildMfaRequirementDismissed;
			this.guildMfaRequirementDismissed = rest;
		}
	}
}

export default new Nagbar();
