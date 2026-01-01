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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {RelationshipTypes} from '~/Constants';
import {ChangeFriendNicknameModal} from '~/components/modals/ChangeFriendNicknameModal';
import type {UserRecord} from '~/records/UserRecord';
import RelationshipStore from '~/stores/RelationshipStore';
import * as RelationshipActionUtils from '~/utils/RelationshipActionUtils';
import {
	AcceptFriendRequestIcon,
	BlockUserIcon,
	CancelFriendRequestIcon,
	EditIcon,
	IgnoreFriendRequestIcon,
	RemoveFriendIcon,
	SendFriendRequestIcon,
} from '../ContextMenuIcons';
import {MenuItem} from '../MenuItem';

interface SendFriendRequestMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const SendFriendRequestMenuItem: React.FC<SendFriendRequestMenuItemProps> = observer(
	({user, onClose: _onClose}) => {
		const {t, i18n} = useLingui();
		const relationshipType = RelationshipStore.getRelationship(user.id)?.type;
		const [submitting, setSubmitting] = React.useState(false);

		const showFriendRequestSent = relationshipType === RelationshipTypes.OUTGOING_REQUEST;

		const handleSendFriendRequest = React.useCallback(async () => {
			if (submitting || showFriendRequestSent) return;
			setSubmitting(true);
			await RelationshipActionUtils.sendFriendRequest(i18n, user.id);
			setSubmitting(false);
		}, [i18n, showFriendRequestSent, submitting, user.id]);

		return (
			<MenuItem
				icon={<SendFriendRequestIcon />}
				onClick={handleSendFriendRequest}
				disabled={submitting || showFriendRequestSent}
				closeOnSelect={false}
			>
				{showFriendRequestSent ? t`Friend Request Sent` : t`Add Friend`}
			</MenuItem>
		);
	},
);

interface AcceptFriendRequestMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const AcceptFriendRequestMenuItem: React.FC<AcceptFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {t, i18n} = useLingui();
	const handleAcceptFriendRequest = React.useCallback(() => {
		onClose();
		RelationshipActionUtils.acceptFriendRequest(i18n, user.id);
	}, [i18n, user.id, onClose]);

	return (
		<MenuItem icon={<AcceptFriendRequestIcon />} onClick={handleAcceptFriendRequest}>
			{t`Accept Friend Request`}
		</MenuItem>
	);
});

interface RemoveFriendMenuItemProps {
	user: UserRecord;
	onClose: () => void;
	danger?: boolean;
}

export const RemoveFriendMenuItem: React.FC<RemoveFriendMenuItemProps> = observer(({user, onClose, danger = true}) => {
	const {t, i18n} = useLingui();
	const handleRemoveFriend = React.useCallback(() => {
		onClose();
		RelationshipActionUtils.showRemoveFriendConfirmation(i18n, user);
	}, [i18n, user, onClose]);

	return (
		<MenuItem icon={<RemoveFriendIcon />} onClick={handleRemoveFriend} danger={danger}>
			{t`Remove Friend`}
		</MenuItem>
	);
});

interface ChangeFriendNicknameMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const ChangeFriendNicknameMenuItem: React.FC<ChangeFriendNicknameMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const relationship = RelationshipStore.getRelationship(user.id);

	const handleChangeNickname = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <ChangeFriendNicknameModal user={user} />));
	}, [onClose, user]);

	if (relationship?.type !== RelationshipTypes.FRIEND) {
		return null;
	}

	return (
		<MenuItem icon={<EditIcon />} onClick={handleChangeNickname}>
			{t`Change Friend Nickname`}
		</MenuItem>
	);
});

interface IgnoreFriendRequestMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const IgnoreFriendRequestMenuItem: React.FC<IgnoreFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {t, i18n} = useLingui();
	const handleIgnoreFriendRequest = React.useCallback(() => {
		onClose();
		RelationshipActionUtils.ignoreFriendRequest(i18n, user.id);
	}, [i18n, user.id, onClose]);

	return (
		<MenuItem icon={<IgnoreFriendRequestIcon />} onClick={handleIgnoreFriendRequest}>
			{t`Ignore Friend Request`}
		</MenuItem>
	);
});

interface CancelFriendRequestMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const CancelFriendRequestMenuItem: React.FC<CancelFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {t, i18n} = useLingui();
	const handleCancelFriendRequest = React.useCallback(() => {
		onClose();
		RelationshipActionUtils.cancelFriendRequest(i18n, user.id);
	}, [i18n, user.id, onClose]);

	return (
		<MenuItem icon={<CancelFriendRequestIcon />} onClick={handleCancelFriendRequest}>
			{t`Cancel Friend Request`}
		</MenuItem>
	);
});

interface BlockUserMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const BlockUserMenuItem: React.FC<BlockUserMenuItemProps> = observer(({user, onClose}) => {
	const {t, i18n} = useLingui();
	const handleBlockUser = React.useCallback(() => {
		onClose();
		RelationshipActionUtils.showBlockUserConfirmation(i18n, user);
	}, [i18n, user, onClose]);

	return (
		<MenuItem icon={<BlockUserIcon />} onClick={handleBlockUser} danger>
			{t`Block`}
		</MenuItem>
	);
});

interface UnblockUserMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const UnblockUserMenuItem: React.FC<UnblockUserMenuItemProps> = observer(({user, onClose}) => {
	const {t, i18n} = useLingui();
	const handleUnblockUser = React.useCallback(() => {
		onClose();
		RelationshipActionUtils.unblockUser(i18n, user.id);
	}, [i18n, user.id, onClose]);

	return (
		<MenuItem icon={<BlockUserIcon />} onClick={handleUnblockUser}>
			{t`Unblock`}
		</MenuItem>
	);
});

interface RelationshipActionMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const RelationshipActionMenuItem: React.FC<RelationshipActionMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const relationship = RelationshipStore.getRelationship(user.id);
	const relationshipType = relationship?.type;

	if (user.bot) {
		if (relationshipType === RelationshipTypes.FRIEND) {
			return <RemoveFriendMenuItem user={user} onClose={onClose} danger={false} />;
		}
		if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
			return <IgnoreFriendRequestMenuItem user={user} onClose={onClose} />;
		}
		if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
			return <CancelFriendRequestMenuItem user={user} onClose={onClose} />;
		}
		return null;
	}

	switch (relationshipType) {
		case RelationshipTypes.FRIEND:
			return <RemoveFriendMenuItem user={user} onClose={onClose} danger={false} />;
		case RelationshipTypes.INCOMING_REQUEST:
			return (
				<>
					<AcceptFriendRequestMenuItem user={user} onClose={onClose} />
					<IgnoreFriendRequestMenuItem user={user} onClose={onClose} />
				</>
			);
		case RelationshipTypes.OUTGOING_REQUEST:
			return (
				<MenuItem icon={<SendFriendRequestIcon />} disabled closeOnSelect={false}>
					{t`Friend Request Sent`}
				</MenuItem>
			);
		case RelationshipTypes.BLOCKED:
			return null;
		default:
			return <SendFriendRequestMenuItem user={user} onClose={onClose} />;
	}
});
