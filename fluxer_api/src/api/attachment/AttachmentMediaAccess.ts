// SPDX-License-Identifier: AGPL-3.0-or-later

import {encodeStorageKey} from '@app/api/attachment/AttachmentMediaUpstream';
import {
	type ApiAttachmentLocation,
	ownAttachmentLocation,
	parseApiAttachmentUrl,
} from '@app/api/attachment/AttachmentPublicUrl';
import {type MessageID, userIdToChannelId} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	attachmentStorageChannelId,
	isOwnedEmbedAttachment,
	makeAttachmentCdnKey,
} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageRequestService} from '@app/api/channel/services/message/MessageRequestService';
import {type ThreadViewer, viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import type {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import {isApiProxiedAttachments} from '@app/api/instance/AttachmentAccessMode';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getFavoriteMemeRepository} from '@app/api/middleware/ServiceSingletons';
import type {Embed} from '@app/api/models/Embed';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {MessageEmbedChildResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	MessageAttachmentResponse,
	MessageResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {attachmentStorageKeyFromUrl} from '@pkgs/media_proxy_utils/src/AttachmentUrlSignature';
import type {Context} from 'hono';

const DECISION_TTL_MS = 15_000;
const DECISION_CACHE_MAX_ENTRIES = 10_000;

interface CachedDecision {
	storageKey: string;
	expiresAt: number;
}

const decisionCache = new Map<string, CachedDecision>();

interface AttachmentMediaAccessRequest {
	user: User;
	viewer: ThreadViewer;
	credentialKey: string;
	location: ApiAttachmentLocation;
	requestCache: RequestCache;
}

interface AttachmentMediaAccessDeps {
	messageRequestService: MessageRequestService;
	channelRepository: ChannelRepository;
	favoriteMemeRepository: FavoriteMemeRepository;
}

function decisionKey(request: AttachmentMediaAccessRequest): string {
	const {location} = request;
	return [
		request.user.id,
		request.credentialKey,
		request.viewer.kind === 'user' && request.viewer.capable ? 'capable' : 'basic',
		location.channelId,
		location.messageId ?? '',
		location.attachmentId,
		location.filename,
	].join('\u0000');
}

function readCachedDecision(key: string): string | null {
	const cached = decisionCache.get(key);
	if (!cached) return null;
	if (cached.expiresAt <= Date.now()) {
		decisionCache.delete(key);
		return null;
	}
	return cached.storageKey;
}

function rememberDecision(key: string, storageKey: string): void {
	decisionCache.delete(key);
	decisionCache.set(key, {storageKey, expiresAt: Date.now() + DECISION_TTL_MS});
	if (decisionCache.size > DECISION_CACHE_MAX_ENTRIES) {
		const oldest = decisionCache.keys().next().value;
		if (oldest !== undefined) decisionCache.delete(oldest);
	}
}

export function clearAttachmentMediaDecisionsForTesting(): void {
	decisionCache.clear();
}

function isServedAttachment(
	attachments: ReadonlyArray<MessageAttachmentResponse> | null | undefined,
	location: ApiAttachmentLocation,
): boolean {
	return (attachments ?? []).some(
		(attachment) =>
			attachment.id === location.attachmentId.toString() &&
			attachment.filename === location.filename &&
			attachment.url != null &&
			attachment.expired !== true,
	);
}

function embedResponseUrls(
	embed: MessageEmbedChildResponse & {children?: Array<MessageEmbedChildResponse> | null},
): Array<string> {
	const media = [embed.image, embed.thumbnail, embed.video, embed.audio];
	return [
		embed.url,
		embed.author?.icon_url,
		embed.author?.proxy_icon_url,
		embed.footer?.icon_url,
		embed.footer?.proxy_icon_url,
		...media.flatMap((entry) => [entry?.url, entry?.proxy_url]),
		...(embed.children ?? []).flatMap((child) => embedResponseUrls(child)),
	].filter((url): url is string => typeof url === 'string');
}

function isServedEmbedUrl(
	embeds: ReadonlyArray<MessageEmbedChildResponse> | null | undefined,
	location: ApiAttachmentLocation,
	messageId: MessageID,
): boolean {
	return (embeds ?? [])
		.flatMap((embed) => embedResponseUrls(embed))
		.map((url) => parseApiAttachmentUrl(url))
		.some(
			(served) =>
				served !== null &&
				served.channelId === location.channelId &&
				served.attachmentId === location.attachmentId &&
				served.filename === location.filename &&
				served.messageId === messageId,
		);
}

function ownedEmbedMediaUrls(embed: Embed): Array<string> {
	return [
		...[embed.image, embed.thumbnail, embed.video, embed.audio]
			.filter((media) => media !== null && isOwnedEmbedAttachment(media))
			.map((media) => media?.url),
		...embed.children.flatMap((child) => ownedEmbedMediaUrls(child)),
	].filter((url): url is string => typeof url === 'string');
}

function hasOwnedEmbedKey(embeds: ReadonlyArray<Embed>, storageKey: string): boolean {
	return embeds
		.flatMap((embed) => ownedEmbedMediaUrls(embed))
		.some((url) => attachmentStorageKeyFromUrl(url, Config.endpoints.media) === storageKey);
}

function storedAttachmentKey(
	message: Message,
	response: MessageResponse,
	location: ApiAttachmentLocation,
	messageId: MessageID,
): string | null {
	const {attachmentId, filename} = location;
	const matches = (attachment: {id: bigint; filename: string}) =>
		attachment.id === attachmentId && attachment.filename === filename;
	const storageChannelId = attachmentStorageChannelId(message);
	if (message.attachments.some(matches) && isServedAttachment(response.attachments, location)) {
		return makeAttachmentCdnKey(storageChannelId, attachmentId, filename);
	}
	const snapshots = response.message_snapshots ?? [];
	if (
		message.messageSnapshots.some((snapshot) => snapshot.attachments.some(matches)) &&
		snapshots.some((snapshot) => isServedAttachment(snapshot.attachments, location))
	) {
		return makeAttachmentCdnKey(message.channelId, attachmentId, filename);
	}
	if (location.messageId === null) return null;
	const ownedKey = makeAttachmentCdnKey(storageChannelId, attachmentId, filename);
	if (hasOwnedEmbedKey(message.embeds, ownedKey) && isServedEmbedUrl(response.embeds, location, messageId)) {
		return ownedKey;
	}
	const snapshotKey = makeAttachmentCdnKey(message.channelId, attachmentId, filename);
	if (
		message.messageSnapshots.some((snapshot) => hasOwnedEmbedKey(snapshot.embeds, snapshotKey)) &&
		snapshots.some((snapshot) => isServedEmbedUrl(snapshot.embeds, location, messageId))
	) {
		return snapshotKey;
	}
	return null;
}

function isHiddenFromViewer(error: unknown): boolean {
	return error instanceof FluxerError && (error.status === 403 || error.status === 404);
}

async function resolveFavoriteMeme(
	deps: AttachmentMediaAccessDeps,
	{user, location}: AttachmentMediaAccessRequest,
): Promise<string | null> {
	const memes = await deps.favoriteMemeRepository.findByUserId(user.id);
	const owned = memes.some(
		(meme) => meme.attachmentId === location.attachmentId && meme.filename === location.filename,
	);
	return owned ? makeAttachmentCdnKey(userIdToChannelId(user.id), location.attachmentId, location.filename) : null;
}

async function resolveUncached(
	deps: AttachmentMediaAccessDeps,
	request: AttachmentMediaAccessRequest,
): Promise<string | null> {
	const {location, user} = request;
	if (location.messageId === null && location.channelId.toString() === user.id.toString()) {
		return resolveFavoriteMeme(deps, request);
	}
	const messageId =
		location.messageId ??
		(await deps.channelRepository.messages.lookupAttachmentByChannelAndFilename(
			location.channelId,
			location.attachmentId,
			location.filename,
		));
	if (messageId === null) return null;
	let response: MessageResponse;
	try {
		response = await deps.messageRequestService.getMessage({
			userId: user.id,
			viewer: request.viewer,
			channelId: location.channelId,
			messageId,
			requestCache: request.requestCache,
		});
	} catch (error) {
		if (isHiddenFromViewer(error)) return null;
		throw error;
	}
	const message = await deps.channelRepository.messages.getMessage(location.channelId, messageId);
	if (!message) return null;
	return storedAttachmentKey(message, response, location, messageId);
}

function credentialKey(ctx: Context<HonoEnv>): string {
	const session = ctx.get('authSession');
	if (ctx.get('authTokenType') === 'bot' || !session) return ctx.get('authTokenType') ?? 'none';
	return Buffer.from(session.sessionIdHash).toString('base64url');
}

export async function resolveAttachmentForRequester(
	ctx: Context<HonoEnv>,
	location: ApiAttachmentLocation,
): Promise<string | null> {
	const request: AttachmentMediaAccessRequest = {
		user: ctx.get('user'),
		viewer: viewerFromCtx(ctx),
		credentialKey: credentialKey(ctx),
		location,
		requestCache: ctx.get('requestCache'),
	};
	const key = decisionKey(request);
	const cached = readCachedDecision(key);
	if (cached !== null) return cached;
	const storageKey = await resolveUncached(
		{
			messageRequestService: ctx.get('messageRequestService'),
			channelRepository: ctx.get('channelRepository'),
			favoriteMemeRepository: getFavoriteMemeRepository(),
		},
		request,
	);
	if (storageKey !== null) rememberDecision(key, storageKey);
	return storageKey;
}

export async function mediaUrlForRequester(ctx: Context<HonoEnv>, url: string): Promise<string | null | undefined> {
	if (!isApiProxiedAttachments()) return undefined;
	const location = ownAttachmentLocation(url);
	if (location === null) return undefined;
	const storageKey = await resolveAttachmentForRequester(ctx, location);
	if (storageKey === null) return null;
	return `${Config.endpoints.media.replace(/\/+$/u, '')}/${encodeStorageKey(storageKey)}`;
}
