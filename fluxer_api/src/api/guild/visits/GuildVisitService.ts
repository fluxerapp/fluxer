// SPDX-License-Identifier: AGPL-3.0-or-later

import {createInviteCode, type GuildID, type UserID, vanityCodeToInviteCode} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import {withThreadParentFieldsMany} from '@app/api/channel/services/thread/ThreadParentSettings';
import type {GuildVisitRow} from '@app/api/database/types/GuildVisitTypes';
import {type ThreadViewer, viewerActive} from '@app/api/experiment/ChannelThreadsGate';
import {userGuildVisitsActive} from '@app/api/experiment/GuildVisitsGate';
import {
	mapGuildEmojiToResponse,
	mapGuildRoleToResponse,
	mapGuildStickerToResponse,
	mapGuildToGuildResponse,
} from '@app/api/guild/GuildModel';
import type {GuildService} from '@app/api/guild/services/GuildService';
import {maskChannelResponseThreadBits} from '@app/api/guild/services/ThreadPermissionBits';
import {assertNotBannedFromGuildVisit, endGuildVisit} from '@app/api/guild/visits/GuildVisitLifecycle';
import {guildVisitJoinTotal, guildVisitStartTotal} from '@app/api/guild/visits/GuildVisitMetrics';
import {
	computeVisitorChannelPermissionsFor,
	forgetGuildVisitRow,
	type GuildVisitorAccess,
	guildVisitRepository,
	isGuildUnavailable,
	resolveGuildVisitorAccess,
	toVisitorChannelModel,
} from '@app/api/guild/visits/GuildVisitorAccess';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {VisitSource} from '@app/api/infrastructure/activity/Contract.generated';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {parseInviteCodeList} from '@app/api/instance/AutoJoinInviteCodes';
import type {SingleCommunityService} from '@app/api/instance/SingleCommunityService';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {
	getChannelRepository,
	getGuildDiscoveryRepository,
	getGuildRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {Guild} from '@app/api/models/Guild';
import type {Invite} from '@app/api/models/Invite';
import type {User} from '@app/api/models/User';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import {assertCanVisit, assertTierAllows, getAccountTier, type TierContext} from '@app/api/user/AccountTier';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {ChannelTypes, InviteTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures, GuildOperations, JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {
	GuildVisitEndReasons,
	type GuildVisitSource,
	GuildVisitSources,
} from '@fluxer/constants/src/GuildVisitConstants';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {GuildVisitNotFoundError} from '@fluxer/errors/src/domains/guild/GuildVisitNotFoundError';
import {GuildVisitsDisabledError} from '@fluxer/errors/src/domains/guild/GuildVisitsDisabledError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {InvitesDisabledError} from '@fluxer/errors/src/domains/invite/InvitesDisabledError';
import {UnknownInviteError} from '@fluxer/errors/src/domains/invite/UnknownInviteError';
import {DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildVisitResponse, GuildVisitStartResponse} from '@fluxer/schema/src/domains/guild/GuildVisitSchemas';

interface VisitStartContext {
	user: User;
	ip: string | null;
}

interface VisitAuthorization {
	guild: Guild;
	source: GuildVisitSource;
	invite: Invite | null;
}

function mapGuildVisitToResponse(row: GuildVisitRow): GuildVisitResponse {
	return {
		guild_id: row.guild_id.toString(),
		source: row.source as GuildVisitResponse['source'],
		started_at: row.started_at.toISOString(),
		opened_at: row.opened_at.toISOString(),
	};
}

function activitySource(source: GuildVisitSource): VisitSource {
	switch (source) {
		case GuildVisitSources.INVITE:
			return 'invite';
		case GuildVisitSources.VANITY_URL:
			return 'vanity';
		case GuildVisitSources.DISCOVERY:
			return 'discovery';
		case GuildVisitSources.OPERATOR:
			return 'operator';
	}
}

export function maxGuildVisitsFor(user: User, limitConfigService: LimitConfigService): number {
	const cap = resolveLimitSafe(
		limitConfigService.getConfigSnapshot(),
		createLimitMatchContext({user}),
		'max_guild_visits',
		DEFAULT_STOCK_LIMITS.max_guild_visits,
	);
	return getAccountTier(user) === 'verified' ? cap : Math.max(1, Math.floor(cap / 2));
}

export class GuildVisitService {
	constructor(
		private readonly gatewayService: IGatewayService,
		private readonly guildService: GuildService,
		private readonly inviteService: InviteService,
		private readonly singleCommunityService: SingleCommunityService,
		private readonly limitConfigService: LimitConfigService,
		private readonly userCacheService: UserCacheService,
	) {}

	async startFromInvite(params: VisitStartContext & {code: string}): Promise<GuildVisitStartResponse> {
		if (!userGuildVisitsActive(params.user.id)) throw new UnknownInviteError();
		const invite = await this.inviteService.findInvite(createInviteCode(params.code));
		if (!invite || invite.type !== InviteTypes.GUILD || !invite.guildId) throw new UnknownInviteError();
		const guild = await this.guildService.data.operations.getGuildSystem(invite.guildId);
		const vanityCode = guild.vanityUrlCode ? vanityCodeToInviteCode(guild.vanityUrlCode) : null;
		const source = invite.code === vanityCode ? GuildVisitSources.VANITY_URL : GuildVisitSources.INVITE;
		return this.start(params, {guild, source, invite});
	}

	async startFromDiscovery(params: VisitStartContext & {guildId: GuildID}): Promise<GuildVisitStartResponse> {
		if (!userGuildVisitsActive(params.user.id)) throw new UnknownGuildError();
		const status = await getGuildDiscoveryRepository().findByGuildId(params.guildId);
		if (status?.status !== DiscoveryApplicationStatus.APPROVED) throw new UnknownGuildError();
		const guild = await this.guildService.data.operations.getGuildSystem(params.guildId);
		return this.start(params, {guild, source: GuildVisitSources.DISCOVERY, invite: null});
	}

	async startForOperator(params: VisitStartContext & {code: string | null}): Promise<GuildVisitStartResponse | null> {
		if (!userGuildVisitsActive(params.user.id)) return null;
		let guildId: GuildID | null;
		let invite: Invite | null = null;
		if (params.code === null) {
			guildId = await this.singleCommunityService.getStockCommunityId();
		} else {
			invite = await this.inviteService.findInvite(createInviteCode(params.code));
			guildId = invite?.type === InviteTypes.GUILD ? invite.guildId : null;
		}
		if (guildId === null) return null;
		const guild = await this.guildService.data.operations.getGuildSystem(guildId);
		return this.start(params, {guild, source: GuildVisitSources.OPERATOR, invite});
	}

	private async start(context: VisitStartContext, auth: VisitAuthorization): Promise<GuildVisitStartResponse> {
		const {user, ip} = context;
		const {guild, source, invite} = auth;
		const guildId = guild.id;
		const viaInvite = source === GuildVisitSources.INVITE || source === GuildVisitSources.VANITY_URL;
		if (viaInvite && (guild.disabledOperations & GuildOperations.INSTANT_INVITES) !== 0) {
			throw new FeatureTemporarilyDisabledError();
		}
		if (source !== GuildVisitSources.OPERATOR && guild.features.has(GuildFeatures.INVITES_DISABLED)) {
			throw new InvitesDisabledError();
		}
		if (await this.gatewayService.hasGuildMember({guildId, userId: user.id})) {
			return {guild_id: guildId.toString(), member: true, visit: null};
		}
		if (isGuildUnavailable(guild)) throw new UnknownGuildError();
		if (source !== GuildVisitSources.OPERATOR && !guild.features.has(GuildFeatures.VISITS_ENABLED)) {
			throw new GuildVisitsDisabledError();
		}
		assertCanVisit(user, guild);
		assertAccountNotLimited(user);
		await assertNotBannedFromGuildVisit({guildId, user, ip});
		const repository = guildVisitRepository();
		const now = new Date();
		const visits = await repository.list(user.id);
		const existing = visits.find((row) => row.guild_id === guildId);
		if (existing) {
			forgetGuildVisitRow(user.id, guildId);
			const touched = await repository.touch(existing, now);
			await this.gatewayService.startVisit({
				userId: user.id,
				guildId,
				operator: existing.source === GuildVisitSources.OPERATOR,
			});
			return {guild_id: guildId.toString(), member: false, visit: mapGuildVisitToResponse(touched)};
		}
		if (invite && invite.maxUses > 0 && invite.uses >= invite.maxUses) throw new UnknownInviteError();
		await this.makeRoomForVisit(user, visits);
		const rowDetails = {
			user_id: user.id,
			guild_id: guildId,
			source,
			invite_code: viaInvite && invite ? invite.code : null,
		};
		let expiresAt: Date | null = null;
		if (viaInvite && invite) {
			expiresAt = invite.maxAge > 0 ? new Date(invite.createdAt.getTime() + invite.maxAge * 1000) : null;
		}
		const row: GuildVisitRow = {
			...rowDetails,
			invite_expires_at: expiresAt,
			started_at: now,
			opened_at: now,
		};
		if (!(await repository.upsert(row, now))) throw new UnknownInviteError();
		forgetGuildVisitRow(user.id, guildId);
		await this.gatewayService.startVisit({userId: user.id, guildId, operator: source === GuildVisitSources.OPERATOR});
		const tier = getAccountTier(user);
		guildVisitStartTotal.inc(`source="${activitySource(source)}",tier="${tier}"`);
		void emitActivity('guild_visit_started', user.id.toString(), {
			user_id: user.id.toString(),
			guild_id: guildId.toString(),
			source: activitySource(source),
			tier,
		});
		return {guild_id: guildId.toString(), member: false, visit: mapGuildVisitToResponse(row)};
	}

	private async makeRoomForVisit(user: User, visits: Array<GuildVisitRow>): Promise<void> {
		const cap = maxGuildVisitsFor(user, this.limitConfigService);
		const ordered = [...visits].sort((left, right) => left.opened_at.getTime() - right.opened_at.getTime());
		let overflow = ordered.length - cap + 1;
		for (const row of ordered) {
			if (overflow <= 0) break;
			await endGuildVisit({
				userId: user.id,
				guildId: row.guild_id,
				reason: GuildVisitEndReasons.REPLACED,
				gateway: this.gatewayService,
			});
			overflow--;
		}
	}

	async listOwn(userId: UserID): Promise<Array<GuildVisitResponse>> {
		if (!userGuildVisitsActive(userId)) return [];
		const now = Date.now();
		const rows = await guildVisitRepository().list(userId);
		return rows
			.filter((row) => row.invite_expires_at === null || row.invite_expires_at.getTime() > now)
			.sort((left, right) => right.opened_at.getTime() - left.opened_at.getTime())
			.map(mapGuildVisitToResponse);
	}

	async leave(userId: UserID, guildId: GuildID): Promise<void> {
		const row = await guildVisitRepository().get(userId, guildId);
		if (!row) {
			await this.endLiveVisitOnly(userId, guildId);
			return;
		}
		await endGuildVisit({userId, guildId, reason: GuildVisitEndReasons.LEFT, gateway: this.gatewayService});
	}

	async join(params: {
		user: User;
		guildId: GuildID;
		tierContext: TierContext;
		requestCache: RequestCache;
	}): Promise<GuildResponse> {
		const {user, guildId, tierContext} = params;
		const row = userGuildVisitsActive(user.id) ? await guildVisitRepository().get(user.id, guildId) : null;
		if (!row) throw new GuildVisitNotFoundError();
		if (await this.gatewayService.hasGuildMember({guildId, userId: user.id})) {
			await endGuildVisit({userId: user.id, guildId, reason: GuildVisitEndReasons.JOINED, notify: false});
			return mapGuildToGuildResponse(await this.guildService.data.operations.getGuildSystem(guildId));
		}
		assertTierAllows(user, TierGatedActions.GUILD_JOIN, tierContext);
		switch (row.source) {
			case GuildVisitSources.INVITE:
			case GuildVisitSources.VANITY_URL:
				await this.joinThroughInvite(row, params);
				break;
			case GuildVisitSources.DISCOVERY:
				await this.joinThroughDiscovery(row, params);
				break;
			default:
				await this.joinAsOperatorVisit(row, params);
		}
		guildVisitJoinTotal.inc(`source="${activitySource(row.source as GuildVisitSource)}"`);
		return mapGuildToGuildResponse(await this.guildService.data.operations.getGuildSystem(guildId));
	}

	private async joinThroughInvite(
		row: GuildVisitRow,
		params: {user: User; tierContext: TierContext; requestCache: RequestCache},
	): Promise<void> {
		const invite = row.invite_code ? await this.inviteService.findInvite(createInviteCode(row.invite_code)) : null;
		if (!invite || invite.guildId !== row.guild_id) {
			await endGuildVisit({
				userId: params.user.id,
				guildId: row.guild_id,
				reason: GuildVisitEndReasons.INVITE_REVOKED,
				gateway: this.gatewayService,
			});
			throw new UnknownInviteError();
		}
		await this.inviteService.acceptInvite({
			userId: params.user.id,
			inviteCode: invite.code,
			requestCache: params.requestCache,
			tierContext: params.tierContext,
		});
	}

	private async joinThroughDiscovery(
		row: GuildVisitRow,
		params: {user: User; tierContext: TierContext; requestCache: RequestCache},
	): Promise<void> {
		const status = await getGuildDiscoveryRepository().findByGuildId(row.guild_id);
		if (status?.status !== DiscoveryApplicationStatus.APPROVED) {
			await endGuildVisit({
				userId: params.user.id,
				guildId: row.guild_id,
				reason: GuildVisitEndReasons.DISABLED,
				gateway: this.gatewayService,
			});
			throw new UnknownGuildError();
		}
		const guild = await this.guildService.data.operations.getGuildSystem(row.guild_id);
		if (guild.features.has(GuildFeatures.INVITES_DISABLED)) throw new InvitesDisabledError();
		await this.guildService.members.addUserToGuild({
			userId: params.user.id,
			guildId: row.guild_id,
			sendJoinMessage: true,
			joinSourceType: JoinSourceTypes.DISCOVERY,
			requestCache: params.requestCache,
			tierContext: params.tierContext,
		});
	}

	private async findOperatorInvite(guildId: GuildID): Promise<Invite | null> {
		for (const code of parseInviteCodeList(Config.instance.autoJoinInviteCode)) {
			const invite = await this.inviteService.findInvite(createInviteCode(code));
			if (invite?.guildId === guildId) return invite;
		}
		return null;
	}

	private async joinAsOperatorVisit(
		row: GuildVisitRow,
		params: {user: User; tierContext: TierContext; requestCache: RequestCache},
	): Promise<void> {
		if ((await this.singleCommunityService.getStockCommunityId()) !== row.guild_id) {
			const invite = await this.findOperatorInvite(row.guild_id);
			if (invite) {
				await this.inviteService.acceptInvite({
					userId: params.user.id,
					inviteCode: invite.code,
					requestCache: params.requestCache,
					tierContext: params.tierContext,
				});
				return;
			}
			await endGuildVisit({
				userId: params.user.id,
				guildId: row.guild_id,
				reason: GuildVisitEndReasons.DISABLED,
				gateway: this.gatewayService,
			});
			throw new UnknownGuildError();
		}
		await this.guildService.members.addUserToGuild({
			userId: params.user.id,
			guildId: row.guild_id,
			skipGuildLimitCheck: true,
			joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
			requestCache: params.requestCache,
			tierContext: params.tierContext,
		});
	}

	async listChannelsForVisitor(params: {
		userId: UserID;
		guildId: GuildID;
		viewer: ThreadViewer;
		requestCache: RequestCache;
	}): Promise<Array<ChannelResponse> | null> {
		const access = await resolveGuildVisitorAccess({userId: params.userId, guildId: params.guildId});
		if (!access) return null;
		return this.visitorChannels(access, params);
	}

	private async visitorChannels(
		access: GuildVisitorAccess,
		params: {guildId: GuildID; viewer: ThreadViewer; requestCache: RequestCache},
	): Promise<Array<ChannelResponse>> {
		const channelRepository = getChannelRepository();
		const channels = await channelRepository.channelData.listGuildChannels(params.guildId, 'enrolled');
		const byId = new Map(channels.map((channel) => [channel.id, channel]));
		const visible = channels.filter((channel) => {
			if (channel.isThreadOnly() && !viewerActive(params.viewer, params.guildId)) return false;
			const parent =
				channel.parentId && channel.type !== ChannelTypes.GUILD_CATEGORY ? (byId.get(channel.parentId) ?? null) : null;
			const permissions = computeVisitorChannelPermissionsFor({access, channel, parent});
			return (permissions & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL;
		});
		const stripped = visible.map(toVisitorChannelModel);
		const responses = await Promise.all(
			stripped.map((channel) =>
				mapChannelToResponse({
					channel,
					currentUserId: null,
					userCacheService: this.userCacheService,
					requestCache: params.requestCache,
				}),
			),
		);
		return maskChannelResponseThreadBits(
			params.guildId,
			params.viewer,
			await withThreadParentFieldsMany(channelRepository.threads, params.guildId, stripped, responses, params.viewer),
		);
	}

	async getGuildForVisitor(params: {
		userId: UserID;
		guildId: GuildID;
		viewer: ThreadViewer;
		requestCache: RequestCache;
	}): Promise<GuildResponse | null> {
		const access = await resolveGuildVisitorAccess({userId: params.userId, guildId: params.guildId});
		if (!access) return null;
		const guildRepository = getGuildRepository();
		const [roles, emojis, stickers, channels] = await Promise.all([
			guildRepository.listRoles(params.guildId),
			guildRepository.listEmojis(params.guildId),
			guildRepository.listStickers(params.guildId),
			this.visitorChannels(access, params),
		]);
		return {
			...mapGuildToGuildResponse(access.view.guild),
			roles: roles.map(mapGuildRoleToResponse),
			emojis: emojis.map(mapGuildEmojiToResponse),
			stickers: stickers.map(mapGuildStickerToResponse),
			channels,
			member_count: access.view.guild.memberCount,
			visiting: true,
		};
	}

	private async endLiveVisitOnly(userId: UserID, guildId: GuildID): Promise<void> {
		try {
			await this.gatewayService.endVisit({userId, guildId});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), guildId: guildId.toString(), error},
				'Failed to end a visit on the gateway',
			);
		}
	}
}
