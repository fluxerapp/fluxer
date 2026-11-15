// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import Authentication from '@app/features/auth/state/Authentication';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {collectWireMessageGuildMemberUserIds} from '@app/features/messaging/utils/MessageMemberLoadUtils';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import UserSettings from '@app/features/user/state/UserSettings';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {when} from 'mobx';

const COZY_AVATAR_SIZE = 40;
const COMPACT_AVATAR_SIZE = 16;
const ROLE_ICON_SIZE = 20;
const MAX_PREFETCHED_IMAGES = 64;
const CUSTOM_EMOJI_PATTERN = /<(a?):[\w~-]+:(\d+)>/g;

function deadline(budgetMs: number): {readonly remaining: () => number} {
	const endsAt = performance.now() + budgetMs;
	return {remaining: () => Math.max(0, endsAt - performance.now())};
}

function settleWithin(promise: Promise<unknown>, timeoutMs: number): Promise<void> {
	if (timeoutMs <= 0) {
		return Promise.resolve();
	}
	let timer: ReturnType<typeof setTimeout> | undefined;
	const timeout = new Promise<void>((resolve) => {
		timer = setTimeout(resolve, timeoutMs);
	});
	return Promise.race([
		promise.then(
			() => undefined,
			() => undefined,
		),
		timeout,
	]).finally(() => clearTimeout(timer));
}

async function settleMembers(guildId: string, userIds: Array<string>, timeoutMs: number): Promise<void> {
	if (userIds.length === 0) {
		return;
	}
	const settled = when(() => userIds.every((userId) => GuildMembers.isMembershipKnown(guildId, userId)));
	void GuildMembers.hydrateMembers(guildId, userIds);
	await settleWithin(settled, timeoutMs);
	settled.cancel();
}

function avatarSize(): number | null {
	if (!UserSettings.getMessageDisplayCompact()) {
		return COZY_AVATAR_SIZE;
	}
	return Accessibility.showUserAvatarsInCompactMode ? COMPACT_AVATAR_SIZE : null;
}

function authorImageUrls(guildId: string | null, message: WireMessage, size: number | null): Array<string> {
	const urls: Array<string> = [];
	const author = message.author;
	const member = guildId === null ? null : GuildMembers.getMember(guildId, author.id);
	if (size !== null) {
		const requestSize = size * getAppRemScale();
		urls.push(
			guildId !== null && member !== null
				? AvatarUtils.getGuildMemberDisplayAvatarURL({
						guildId,
						user: {id: author.id, avatar: author.avatar},
						memberAvatar: member.avatar,
						avatarUnset: member.isAvatarUnset(),
						size: requestSize,
					})
				: AvatarUtils.getUserAvatarURL({id: author.id, avatar: author.avatar}, false, requestSize),
		);
	}
	const iconRole = member?.getIconRole();
	if (iconRole?.icon) {
		urls.push(AvatarUtils.getRoleIconURL({id: iconRole.id, icon: iconRole.icon}, ROLE_ICON_SIZE));
	}
	for (const match of message.content.matchAll(CUSTOM_EMOJI_PATTERN)) {
		urls.push(buildCustomEmojiURL({id: match[2], animated: match[1] === 'a'}));
	}
	return urls;
}

function loadImages(urls: ReadonlySet<string>, timeoutMs: number): Promise<void> {
	const pending = Array.from(urls)
		.filter((url) => url.length > 0 && !ImageCacheUtils.hasImage(url))
		.slice(0, MAX_PREFETCHED_IMAGES)
		.map(
			(url) =>
				new Promise<void>((resolve) => {
					ImageCacheUtils.loadImage(url, resolve, resolve);
				}),
		);
	return settleWithin(Promise.all(pending), timeoutMs);
}

export async function prefetchMessageAuthors(
	guildId: string | null,
	messages: ReadonlyArray<WireMessage>,
	budgetMs: number,
): Promise<void> {
	if (messages.length === 0) {
		return;
	}
	const budget = deadline(budgetMs);
	if (guildId !== null) {
		const userIds = collectWireMessageGuildMemberUserIds(messages, Authentication.currentUserId);
		await settleMembers(guildId, userIds, budget.remaining());
	}
	const size = avatarSize();
	const urls = new Set<string>();
	for (const message of messages) {
		for (const url of authorImageUrls(guildId, message, size)) {
			urls.add(url);
		}
	}
	await loadImages(urls, budget.remaining());
}
