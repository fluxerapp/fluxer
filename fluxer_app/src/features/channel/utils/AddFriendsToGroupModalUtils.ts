// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import {
	showGroupInviteCreateFailedModal,
	showGroupRecipientAddFailedModal,
} from '@app/features/channel/components/alerts/GroupDmRecipientErrorModalUtils';
import Channels from '@app/features/channel/state/Channels';
import {getGroupDmRemainingSlots} from '@app/features/channel/utils/GroupDmUtils';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {blockIfAccountLimited} from '@app/features/user/utils/AccountLimitUtils';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {msg} from '@lingui/core/macro';
import {useCallback, useMemo, useRef, useState} from 'react';

const FAILED_TO_COPY_INVITE_LINK_DESCRIPTOR = msg({
	message: 'Failed to copy invite link',
	comment: 'Error message in the add friends to group modal utils helper.',
});
const logger = new Logger('AddFriendsToGroupModalUtils');

interface InviteCacheEntry {
	code: string;
	expiresAt: number;
}

interface State {
	selectedUserIds: Array<string>;
	searchQuery: string;
	inviteLink: string | null;
	isAdding: boolean;
	isGeneratingInvite: boolean;
	currentMemberIds: Array<string>;
	remainingSlotsCount: number;
	availableFriendsCount: number;
}

interface Handlers {
	handleToggle: (userId: string) => void;
	handleAddFriends: () => Promise<void>;
	handleGenerateInvite: () => Promise<string | null>;
	handleGenerateOrCopyInvite: () => Promise<boolean>;
	setSearchQuery: (query: string) => void;
}

export function useAddFriendsToGroupModalLogic(channelId: string): State & Handlers {
	const [selectedUserIds, setSelectedUserIds] = useState<Array<string>>([]);
	const [searchQuery, setSearchQuery] = useState('');
	const [inviteLink, setInviteLink] = useState<string | null>(null);
	const [isAdding, setIsAdding] = useState(false);
	const [isGeneratingInvite, setIsGeneratingInvite] = useState(false);
	const inviteCacheRef = useRef<Map<string, InviteCacheEntry>>(new Map());
	const channel = Channels.getChannel(channelId);
	const currentMemberIds = useMemo(() => Array.from(channel?.recipientIds ?? []), [channel?.recipientIds]);
	const remainingSlotsCount = getGroupDmRemainingSlots(channel);
	const availableFriendsCount = 0;
	const handleToggle = useCallback(
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
	const handleAddFriends = useCallback(async () => {
		if (blockIfAccountLimited()) return;
		setIsAdding(true);
		try {
			const promises = selectedUserIds.map((userId) =>
				PrivateChannelCommands.addRecipient(channelId, userId).catch((error) => {
					logger.error(`Failed to add recipient ${userId}:`, error);
					showGroupRecipientAddFailedModal(error);
				}),
			);
			await Promise.all(promises);
			setSelectedUserIds([]);
		} finally {
			setIsAdding(false);
		}
	}, [selectedUserIds, channelId]);
	const handleGenerateInvite = useCallback(async (): Promise<string | null> => {
		setIsGeneratingInvite(true);
		try {
			const cached = inviteCacheRef.current.get(channelId);
			const now = Date.now();
			const EXPIRATION_TIME = MS_PER_DAY;
			if (cached && cached.expiresAt > now) {
				const cachedLink = `${RuntimeConfig.inviteEndpoint}/${cached.code}`;
				setInviteLink(cachedLink);
				return cachedLink;
			}
			const invite = await InviteCommands.create(channelId, {max_age: 86400});
			const fullUrl = `${RuntimeConfig.inviteEndpoint}/${invite.code}`;
			inviteCacheRef.current.set(channelId, {
				code: invite.code,
				expiresAt: now + EXPIRATION_TIME,
			});
			setInviteLink(fullUrl);
			return fullUrl;
		} catch (error) {
			logger.error('Failed to generate invite:', error);
			showGroupInviteCreateFailedModal(error);
			return null;
		} finally {
			setIsGeneratingInvite(false);
		}
	}, [channelId]);
	const handleGenerateOrCopyInvite = useCallback(async (): Promise<boolean> => {
		const linkToCopy = inviteLink ?? (await handleGenerateInvite());
		if (!linkToCopy) {
			return false;
		}
		const copied = await TextCopyCommands.copy(i18n, linkToCopy, true);
		if (!copied) {
			showChannelErrorModal({
				title: i18n._(FAILED_TO_COPY_INVITE_LINK_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'channel.add-friends-to-group-modal-utils.copy-invite-link-failed.generic-error-modal',
			});
		}
		return copied;
	}, [inviteLink, handleGenerateInvite]);
	return {
		selectedUserIds,
		searchQuery,
		inviteLink,
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
	};
}
