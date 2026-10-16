// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {ContextMenuTargetElement} from '@app/features/ui/state/ContextMenu';
import ContextMenu, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import type * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {autorun} from 'mobx';
import type React from 'react';
import {useEffect, useState} from 'react';

export const NOTE_MIN_ROWS = 2;
export const NOTE_MAX_ROWS = 8;

export type ProfileTab = 'overview' | 'mutual_friends' | 'mutual_communities_groups';

export interface UserInfoProps {
	user: User;
	profile: Profile;
	guildId?: string;
	showProfileDataWarning?: boolean;
}

export interface UserNoteEditorProps {
	userId: string;
	initialNote: string | null;
	autoFocus?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
}

export interface ProfileContentProps {
	profile: Profile;
	user: User;
	userNote: string | null;
	autoFocusNote?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
}

export interface ProfileMediaHeaderProps {
	user: User;
	profile: Profile;
	profileContext: ProfileDisplayUtils.ProfileDisplayContext;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
	bannerColor: string;
	bannerUrl: string | null;
	hoverBannerUrl?: string | null;
	avatarUrl: string | null;
	hoverAvatarUrl?: string | null;
	renderActionButtons: () => React.ReactNode;
}

export interface ProfileBodyProps {
	profile: Profile;
	user: User;
	userNote: string | null;
	autoFocusNote?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
	showProfileDataWarning?: boolean;
}

export interface ProfileModalContentProps {
	profile: Profile;
	user: User;
	userNote: string | null;
	autoFocusNote?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
	renderActionButtons: () => React.ReactNode;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
	showProfileDataWarning?: boolean;
}

export function isKeyboardContextMenuTrigger(event: React.KeyboardEvent<HTMLElement>): boolean {
	return isKeyboardActivationKey(event.key) || event.key === 'ContextMenu' || (event.shiftKey && event.key === 'F10');
}

export function useContextMenuTarget(): ContextMenuTargetElement | null {
	const [contextMenuTarget, setContextMenuTarget] = useState<ContextMenuTargetElement | null>(null);
	useEffect(() => {
		const disposer = autorun(() => {
			const contextMenu = ContextMenu.contextMenu;
			setContextMenuTarget(contextMenu?.target.target ?? null);
		});
		return () => {
			disposer();
		};
	}, []);
	return contextMenuTarget;
}

export function isContextMenuOpenForTarget(
	contextMenuTarget: ContextMenuTargetElement | null,
	target: EventTarget | null,
): boolean {
	if (!contextMenuTarget || !target) {
		return false;
	}
	if (target === contextMenuTarget) {
		return true;
	}
	if (target instanceof Node && isContextMenuNodeTarget(contextMenuTarget)) {
		return target.contains(contextMenuTarget);
	}
	return false;
}
