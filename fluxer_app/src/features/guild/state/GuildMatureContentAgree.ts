// SPDX-License-Identifier: AGPL-3.0-or-later

import GeoIP from '@app/features/app/state/GeoIP';
import Channels from '@app/features/channel/state/Channels';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import Guilds from '@app/features/guild/state/Guilds';
import {
	getEffectiveChannelContentWarning,
	resolveEffectiveChannelMatureContent,
} from '@app/features/messaging/utils/ContentWarningUtils';
import {getEffectiveMatureContentGeoContext} from '@app/features/moderation/utils/MatureContentGeoUtils';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {GuildNsfwAgreementsSchema as GuildMatureContentAgreementsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

const LOCAL_STORAGE_KEY = 'GuildMatureContentAgreeLocal';
let localPersisted = false;

export enum MatureContentGateReason {
	NONE = 0,
	GEO_RESTRICTED = 1,
	MATURE_CONTENT_CHECK_REQUIRED = 2,
	CONSENT_REQUIRED = 3,
}

export type AgreementScope = 'channel' | 'category' | 'guild';

interface MatureContentGateContext {
	channelId?: string | null;
	guildId?: string | null;
}

export interface ResolvedGateContext {
	channelId: string | null;
	categoryId: string | null;
	guildId: string | null;
	effectiveMatureContent: boolean;
	matureContentSource: 'channel' | 'parent' | 'guild' | 'none';
	effectiveWarningLevel: number;
	effectiveWarningText: string | null;
	warningSource: 'channel' | 'parent' | 'guild' | 'none';
	scope: AgreementScope;
	scopeId: string | null;
}

type GateSource = ResolvedGateContext['matureContentSource'];

const GATE_SOURCE_SPECIFICITY: Record<GateSource, number> = {
	none: 0,
	guild: 1,
	parent: 2,
	channel: 3,
};

function getMoreSpecificGateSource(a: GateSource, b: GateSource): GateSource {
	return GATE_SOURCE_SPECIFICITY[b] > GATE_SOURCE_SPECIFICITY[a] ? b : a;
}

class GuildMatureContentAgree {
	agreedChannelIds: Array<string> = [];
	agreedCategoryIds: Array<string> = [];
	agreedGuildIds: Array<string> = [];
	localAgreedChannelIds: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		if (AppStorage.getItem(LOCAL_STORAGE_KEY) != null) this.persistLocal();
		await makeSyncedField(this, {
			field: 'guildNsfwAgreements',
			schema: GuildMatureContentAgreementsSchema,
			persist: ['agreedChannelIds', 'agreedCategoryIds', 'agreedGuildIds'],
			toMessage: (s) => ({
				agreedChannelIds: [...s.agreedChannelIds],
				agreedCategoryIds: [...s.agreedCategoryIds],
				agreedGuildIds: [...s.agreedGuildIds],
			}),
			applyMessage: (s, m) => {
				s.agreedChannelIds = [...m.agreedChannelIds];
				s.agreedCategoryIds = [...m.agreedCategoryIds];
				s.agreedGuildIds = [...m.agreedGuildIds];
			},
		});
	}

	agreeToChannel(channelId: string): void {
		const channel = Channels.getChannel(channelId);
		if (channel?.isThread() && channel.parentId) {
			this.agreeToChannel(channel.parentId);
			return;
		}
		if (channel?.isThreadOnly()) {
			this.persistLocal();
			if (!this.localAgreedChannelIds.includes(channelId)) {
				this.localAgreedChannelIds.push(channelId);
			}
			return;
		}
		if (!this.agreedChannelIds.includes(channelId)) {
			this.agreedChannelIds.push(channelId);
		}
	}

	private persistLocal(): void {
		if (localPersisted) return;
		localPersisted = true;
		void makePersistent(this, LOCAL_STORAGE_KEY, ['localAgreedChannelIds']);
	}

	agreeToCategory(categoryId: string): void {
		if (!this.agreedCategoryIds.includes(categoryId)) {
			this.agreedCategoryIds.push(categoryId);
		}
	}

	agreeToGuild(guildId: string): void {
		if (!this.agreedGuildIds.includes(guildId)) {
			this.agreedGuildIds.push(guildId);
		}
	}

	revokeChannel(channelId: string): void {
		this.localAgreedChannelIds = this.localAgreedChannelIds.filter((id) => id !== channelId);
		this.agreedChannelIds = this.agreedChannelIds.filter((id) => id !== channelId);
	}

	revokeCategory(categoryId: string): void {
		this.agreedCategoryIds = this.agreedCategoryIds.filter((id) => id !== categoryId);
	}

	revokeGuild(guildId: string): void {
		this.agreedGuildIds = this.agreedGuildIds.filter((id) => id !== guildId);
	}

	hasAgreedToChannel(channelId: string): boolean {
		return this.agreedChannelIds.includes(channelId) || this.localAgreedChannelIds.includes(channelId);
	}

	hasAgreedToCategory(categoryId: string): boolean {
		return this.agreedCategoryIds.includes(categoryId);
	}

	hasAgreedToGuild(guildId: string): boolean {
		return this.agreedGuildIds.includes(guildId);
	}

	private resolveContext(context: MatureContentGateContext): ResolvedGateContext {
		const requestedChannel = context.channelId ? Channels.getChannel(context.channelId) : null;
		const threadParentId = requestedChannel?.isThread() ? requestedChannel.parentId : null;
		const channelId = threadParentId ?? context.channelId ?? null;
		const guildIdFromArg = context.guildId ?? null;
		const channel = threadParentId ? Channels.getChannel(threadParentId) : requestedChannel;
		const guildId = guildIdFromArg ?? channel?.guildId ?? null;
		const guild = guildId ? Guilds.getGuild(guildId) : null;
		let categoryId: string | null = null;
		if (channel) {
			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				categoryId = channel.id;
			} else if (channel.parentId) {
				const parent = Channels.getChannel(channel.parentId);
				if (parent && parent.type === ChannelTypes.GUILD_CATEGORY) {
					categoryId = parent.id;
				}
			}
		}
		let effectiveMatureContent = false;
		let matureContentSource: ResolvedGateContext['matureContentSource'] = 'none';
		let effectiveWarningLevel: number = ContentWarningLevel.INHERIT;
		let effectiveWarningText: string | null = null;
		let warningSource: ResolvedGateContext['warningSource'] = 'none';
		if (channel) {
			const matureContentResult = resolveEffectiveChannelMatureContent(channel, guild);
			const warning = getEffectiveChannelContentWarning(channel, guild);
			effectiveMatureContent = matureContentResult.value;
			matureContentSource = matureContentResult.source;
			effectiveWarningLevel = warning.level;
			effectiveWarningText = warning.text;
			warningSource = warning.source;
		} else if (guild) {
			effectiveMatureContent = guild.nsfw;
			matureContentSource = 'guild';
			effectiveWarningLevel = guild.contentWarningLevel ?? ContentWarningLevel.INHERIT;
			effectiveWarningText = guild.contentWarningText ?? null;
			warningSource = guild.contentWarningLevel !== ContentWarningLevel.INHERIT ? 'guild' : 'none';
		}
		let sourceForScope: GateSource = 'none';
		if (effectiveMatureContent) {
			sourceForScope = matureContentSource;
		}
		if (effectiveWarningLevel === ContentWarningLevel.CONTENT_WARNING) {
			sourceForScope = getMoreSpecificGateSource(sourceForScope, warningSource);
		}
		let scope: AgreementScope = 'channel';
		let scopeId: string | null = channelId;
		if (sourceForScope === 'guild') {
			scope = 'guild';
			scopeId = guildId;
		} else if (sourceForScope === 'parent') {
			scope = 'category';
			scopeId = categoryId;
		} else if (sourceForScope === 'channel') {
			scope = 'channel';
			if (channel && channel.type === ChannelTypes.GUILD_CATEGORY) {
				scope = 'category';
				scopeId = channel.id;
			} else {
				scopeId = channelId;
			}
		}
		return {
			channelId,
			categoryId,
			guildId,
			effectiveMatureContent,
			matureContentSource,
			effectiveWarningLevel,
			effectiveWarningText,
			warningSource,
			scope,
			scopeId,
		};
	}

	getResolvedContext(context: MatureContentGateContext): ResolvedGateContext {
		return this.resolveContext(context);
	}

	isGatedContent(context: MatureContentGateContext): boolean {
		const {effectiveMatureContent, effectiveWarningLevel} = this.resolveContext(context);
		return effectiveMatureContent || effectiveWarningLevel === ContentWarningLevel.CONTENT_WARNING;
	}

	private hasEffectiveAgreement(resolved: ResolvedGateContext): boolean {
		if (resolved.scope === 'guild') {
			const guildId = resolved.scopeId ?? resolved.guildId;
			return guildId ? this.hasAgreedToGuild(guildId) : false;
		}
		if (resolved.scope === 'category') {
			const categoryId = resolved.scopeId ?? resolved.categoryId;
			return categoryId ? this.hasAgreedToCategory(categoryId) : false;
		}
		const channelId = resolved.scopeId ?? resolved.channelId;
		return channelId ? this.hasAgreedToChannel(channelId) : false;
	}

	getGateReason(context: MatureContentGateContext): MatureContentGateReason {
		const mockReason = DeveloperOptions.mockMatureContentGateReason;
		if (mockReason !== 'none') {
			switch (mockReason) {
				case 'geo_restricted':
					return MatureContentGateReason.GEO_RESTRICTED;
				case 'mature_content_check_required':
					return MatureContentGateReason.MATURE_CONTENT_CHECK_REQUIRED;
				case 'consent_required':
					return MatureContentGateReason.CONSENT_REQUIRED;
			}
		}
		const resolved = this.resolveContext(context);
		const hasWarning = resolved.effectiveWarningLevel === ContentWarningLevel.CONTENT_WARNING;
		if (!resolved.effectiveMatureContent && !hasWarning) {
			return MatureContentGateReason.NONE;
		}
		if (resolved.effectiveMatureContent) {
			const currentUser = Users.getCurrentUser();
			const {countryCode, regionCode} = getEffectiveMatureContentGeoContext();
			const matureContentCheckGeos = GeoIP.ageRestrictedGeos;
			if (countryCode) {
				const requiresMatureContentCheck = matureContentCheckGeos.some((geo) => {
					if (geo.countryCode !== countryCode) return false;
					if (geo.regionCode === null) return true;
					return geo.regionCode === regionCode;
				});
				if (requiresMatureContentCheck && !currentUser?.matureContentCheckComplete) {
					return MatureContentGateReason.GEO_RESTRICTED;
				}
			}
			if (currentUser && !currentUser.matureContentAllowed) {
				return MatureContentGateReason.MATURE_CONTENT_CHECK_REQUIRED;
			}
		}
		if (this.hasEffectiveAgreement(resolved)) {
			return MatureContentGateReason.NONE;
		}
		return MatureContentGateReason.CONSENT_REQUIRED;
	}

	shouldShowGate(context: MatureContentGateContext): boolean {
		return this.getGateReason(context) !== MatureContentGateReason.NONE;
	}

	getGuildLevelGateReason(guildId: string): MatureContentGateReason {
		return this.getGateReason({guildId, channelId: null});
	}
}

export default new GuildMatureContentAgree();
