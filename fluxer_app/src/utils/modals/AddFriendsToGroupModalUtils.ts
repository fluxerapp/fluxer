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
import React from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import i18n from '~/i18n';
import ChannelStore from '~/stores/ChannelStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {MAX_GROUP_DM_RECIPIENTS} from '~/utils/groupDmUtils';

interface InviteCacheEntry {
	code: string;
	expiresAt: number;
}

interface State {
	selectedUserIds: Array<string>;
	searchQuery: string;
	inviteLink: string | null;
	inviteLinkCopied: boolean;
	isAdding: boolean;
	isGeneratingInvite: boolean;
	currentMemberIds: Array<string>;
	remainingSlotsCount: number;
	availableFriendsCount: number;
}

interface Handlers {
	handleToggle: (userId: string) => void;
	handleAddFriends: () => Promise<void>;
	handleGenerateInvite: () => Promise<void>;
	handleGenerateOrCopyInvite: () => Promise<void>;
	setSearchQuery: (query: string) => void;
	setCopiedState: (copied: boolean) => void;
}

export const useAddFriendsToGroupModalLogic = (channelId: string): State & Handlers => {
	const [selectedUserIds, setSelectedUserIds] = React.useState<Array<string>>([]);
	const [searchQuery, setSearchQuery] = React.useState('');
	const [inviteLink, setInviteLink] = React.useState<string | null>(null);
	const [inviteLinkCopied, setInviteLinkCopied] = React.useState(false);
	const [isAdding, setIsAdding] = React.useState(false);
	const [isGeneratingInvite, setIsGeneratingInvite] = React.useState(false);

	const inviteCacheRef = React.useRef<Map<string, InviteCacheEntry>>(new Map());

	const channel = ChannelStore.getChannel(channelId);
	const currentMemberIds = React.useMemo(() => Array.from(channel?.recipientIds ?? []), [channel?.recipientIds]);

	const remainingSlotsCount = Math.max(MAX_GROUP_DM_RECIPIENTS - currentMemberIds.length, 0);

	const availableFriendsCount = 0;

	const handleToggle = React.useCallback(
		(userId: string) => {
			setSelectedUserIds((prev) => {
				if (prev.includes(userId)) {
					return prev.filter((id) => id !== userId);
				}

				if (prev.length >= remainingSlotsCount) {
					return prev;
				}

				return [...prev, userId];
			});
		},
		[remainingSlotsCount],
	);

	const handleAddFriends = React.useCallback(async () => {
		setIsAdding(true);
		try {
			const promises = selectedUserIds.map((userId) =>
				PrivateChannelActionCreators.addRecipient(channelId, userId).catch((error) => {
					console.error(`Failed to add recipient ${userId}:`, error);
					ToastActionCreators.error(i18n._(msg`Failed to add friend to group`));
				}),
			);

			await Promise.all(promises);

			setSelectedUserIds([]);
		} finally {
			setIsAdding(false);
		}
	}, [selectedUserIds, channelId]);

	const handleGenerateInvite = React.useCallback(async () => {
		setIsGeneratingInvite(true);
		try {
			const cached = inviteCacheRef.current.get(channelId);
			const now = Date.now();
			const EXPIRATION_TIME = 24 * 60 * 60 * 1000;

			if (cached && cached.expiresAt > now) {
				setInviteLink(`${RuntimeConfigStore.inviteEndpoint}/${cached.code}`);
				return;
			}

			const invite = await InviteActionCreators.create(channelId, {max_age: 86400});
			const fullUrl = `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`;

			inviteCacheRef.current.set(channelId, {
				code: invite.code,
				expiresAt: now + EXPIRATION_TIME,
			});

			setInviteLink(fullUrl);
		} catch (error) {
			console.error('Failed to generate invite:', error);
			ToastActionCreators.error(i18n._(msg`Failed to generate invite link`));
		} finally {
			setIsGeneratingInvite(false);
		}
	}, [channelId]);

	const setCopiedState = React.useCallback((copied: boolean) => {
		setInviteLinkCopied(copied);
	}, []);

	const handleGenerateOrCopyInvite = React.useCallback(async () => {
		if (!inviteLink) {
			await handleGenerateInvite();
		}
		if (inviteLink) {
			try {
				await navigator.clipboard.writeText(inviteLink);
			} catch (error) {
				console.error('Failed to copy invite link:', error);
				ToastActionCreators.error(i18n._(msg`Failed to copy invite link`));
			}
		}
	}, [inviteLink, handleGenerateInvite]);

	return {
		selectedUserIds,
		searchQuery,
		inviteLink,
		inviteLinkCopied,
		isAdding,
		isGeneratingInvite,
		currentMemberIds,
		remainingSlotsCount,
		availableFriendsCount,
		handleToggle,
		handleAddFriends,
		handleGenerateInvite,
		handleGenerateOrCopyInvite,
		setSearchQuery,
		setCopiedState,
	};
};
