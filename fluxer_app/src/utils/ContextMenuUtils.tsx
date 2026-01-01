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

import {msg} from '@lingui/core/macro';
import type React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {GuildMemberContextMenu} from '~/components/uikit/ContextMenu/GuildMemberContextMenu';
import {UserContextMenu} from '~/components/uikit/ContextMenu/UserContextMenu';
import i18n from '~/i18n';
import type {MuteConfig} from '~/records/UserGuildSettingsRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import UserStore from '~/stores/UserStore';
import {isLegacyDocument} from '~/types/browser';
import {getFormattedDateTime} from '~/utils/DateUtils';

function getSelectionText(): string {
	let text = '';
	if (window.getSelection) {
		text = window.getSelection()?.toString() || '';
	} else if (isLegacyDocument(document) && document.selection && document.selection.type !== 'Control') {
		text = document.selection.createRange().text;
	}
	return text;
}

function findUserData(element: HTMLElement): {userId?: string; guildId?: string; channelId?: string} {
	let current: HTMLElement | null = element;

	while (current) {
		const userId = current.dataset.userId || current.getAttribute('data-user-id');
		const guildId = current.dataset.guildId || current.getAttribute('data-guild-id');
		const channelId = current.dataset.channelId || current.getAttribute('data-channel-id');

		if (userId) {
			return {userId, guildId: guildId || undefined, channelId: channelId || undefined};
		}

		current = current.parentElement;
	}

	return {};
}

export function handleContextMenu(e: MouseEvent): void {
	const target = e.target as HTMLElement;
	const {userId, guildId, channelId} = findUserData(target);

	if (userId) {
		const user = UserStore.getUser(userId);
		if (user) {
			e.preventDefault();
			e.stopPropagation();

			const isGuildMember = guildId ? GuildMemberStore.getMember(guildId, user.id) : null;

			const view = (e.view ?? null) as unknown as React.MouseEvent<HTMLElement>['view'];
			const reactEvent = {
				nativeEvent: e,
				currentTarget: target,
				target: target,
				pageX: e.pageX,
				pageY: e.pageY,
				preventDefault: () => e.preventDefault(),
				stopPropagation: () => e.stopPropagation(),
				altKey: e.altKey,
				button: e.button,
				buttons: e.buttons,
				clientX: e.clientX,
				clientY: e.clientY,
				ctrlKey: e.ctrlKey,
				metaKey: e.metaKey,
				shiftKey: e.shiftKey,
				screenX: e.screenX,
				screenY: e.screenY,
				detail: e.detail,
				bubbles: e.bubbles,
				cancelable: e.cancelable,
				defaultPrevented: e.defaultPrevented,
				eventPhase: e.eventPhase,
				isTrusted: e.isTrusted,
				movementX: e.movementX,
				movementY: e.movementY,
				relatedTarget: e.relatedTarget,
				timeStamp: e.timeStamp,
				type: e.type,
				view,
				getModifierState: e.getModifierState.bind(e),
				isDefaultPrevented: () => e.defaultPrevented,
				isPropagationStopped: () => false,
				persist: () => {},
			} satisfies React.MouseEvent<HTMLElement>;

			ContextMenuActionCreators.openFromEvent(reactEvent, ({onClose}) =>
				guildId && isGuildMember ? (
					<GuildMemberContextMenu user={user} onClose={onClose} guildId={guildId} channelId={channelId} />
				) : (
					<UserContextMenu user={user} onClose={onClose} guildId={guildId} channelId={channelId} />
				),
			);
			return;
		}
	}

	const selectedText = getSelectionText();
	let href: string | null = null;
	let src: string | null = null;

	let node: HTMLElement | null = target;
	while (node) {
		if (node instanceof HTMLAnchorElement) {
			href = node.href;
		}
		if (node instanceof HTMLImageElement) {
			src = node.src;
		}
		node = node.parentElement;
	}

	if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA') {
		return;
	}

	if (selectedText) {
		return;
	}

	if (href || src) {
		return;
	}

	e.preventDefault();
}

export function getMutedText(isMuted: boolean, muteConfig?: MuteConfig): string | undefined {
	if (!isMuted) return;
	const now = Date.now();
	if (muteConfig?.end_time && new Date(muteConfig.end_time).getTime() <= now) {
		return;
	}
	if (muteConfig?.end_time) {
		return i18n._(msg`Muted until ${getFormattedDateTime(new Date(muteConfig.end_time))}`);
	}
	return i18n._(msg`Muted`);
}

export function getNotificationSettingsLabel(currentNotificationLevel: number): string | undefined {
	switch (currentNotificationLevel) {
		case 0:
			return i18n._(msg`All Messages`);
		case 1:
			return i18n._(msg`Only @mentions`);
		case 2:
			return i18n._(msg`Nothing`);
		case 3:
			return i18n._(msg`Use Category Default`);
		default:
			return;
	}
}
