// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isLegacyDocument} from '@app/features/platform/types/Browser';
import {GuildMemberContextMenu} from '@app/features/ui/action_menu/GuildMemberContextMenu';
import {UserContextMenu} from '@app/features/ui/action_menu/UserContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import type {MuteConfig} from '@app/features/user/models/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {getFormattedDateTime} from '@app/features/user/utils/DateFormatting';
import {msg} from '@lingui/core/macro';
import type React from 'react';
import type {AbstractView} from 'react';

const MUTED_UNTIL_DESCRIPTOR = msg({
	message: 'Muted until {mutedUntil}',
	comment: 'Mute status label that includes a localized expiry time.',
});
const MUTED_DESCRIPTOR = msg({
	message: 'Muted',
	comment: 'Status label indicating the surface is currently muted.',
});
const USE_CATEGORY_DEFAULT_DESCRIPTOR = msg({
	message: 'Use category default',
	comment: 'Option label that inherits the setting value from the parent category.',
});

function toAbstractView(view: Window | null): AbstractView | null {
	if (view === null) return null;
	return view;
}

function getSelectionText(): string {
	if (window.getSelection) {
		return window.getSelection()?.toString() || '';
	}
	const doc = document;
	if (isLegacyDocument(doc) && doc.selection && doc.selection.type !== 'Control') {
		return doc.selection.createRange().text;
	}
	return '';
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

function isEditableContextMenuTarget(target: HTMLElement): boolean {
	let current: HTMLElement | null = target;
	while (current != null) {
		const contentEditable = current.getAttribute('contenteditable')?.toLowerCase();
		if (
			contentEditable === '' ||
			contentEditable === 'true' ||
			contentEditable === 'plaintext-only' ||
			current.isContentEditable
		) {
			return true;
		}
		current = current.parentElement;
	}
	return false;
}

export function handleContextMenu(e: MouseEvent): void {
	const target = e.target as HTMLElement;
	const {userId, guildId, channelId} = findUserData(target);
	if (userId) {
		const user = Users.getUser(userId);
		if (user) {
			e.preventDefault();
			e.stopPropagation();
			const view = toAbstractView(e.view) ?? window;
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
			ContextMenuCommands.openFromEvent(reactEvent, ({onClose}) =>
				guildId ? (
					<GuildMemberContextMenu
						user={user}
						onClose={onClose}
						guildId={guildId}
						channelId={channelId}
						data-flx="lib.overlay.context-menu.handle-context-menu.guild-member-context-menu"
					/>
				) : (
					<UserContextMenu
						user={user}
						onClose={onClose}
						guildId={guildId}
						channelId={channelId}
						data-flx="lib.overlay.context-menu.handle-context-menu.user-context-menu"
					/>
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
	if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA' || isEditableContextMenuTarget(target)) {
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
		const mutedUntil = getFormattedDateTime(new Date(muteConfig.end_time));
		return i18n._(MUTED_UNTIL_DESCRIPTOR, {mutedUntil});
	}
	return i18n._(MUTED_DESCRIPTOR);
}

export function getNotificationSettingsLabel(currentNotificationLevel: number): string | undefined {
	switch (currentNotificationLevel) {
		case 0:
			return i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR);
		case 1:
			return i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR);
		case 2:
			return i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR);
		case 3:
			return i18n._(USE_CATEGORY_DEFAULT_DESCRIPTOR);
		default:
			return;
	}
}
