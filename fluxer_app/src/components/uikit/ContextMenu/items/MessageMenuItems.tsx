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
import {isEmbedsSuppressed, triggerAddReaction} from '~/components/channel/messageActionUtils';
import {MessageDebugModal} from '~/components/debug/MessageDebugModal';
import type {MessageRecord} from '~/records/MessageRecord';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import * as TtsUtils from '~/utils/TtsUtils';
import {
	AddReactionIcon,
	BookmarkIcon,
	CopyIdIcon,
	CopyLinkIcon,
	CopyTextIcon,
	DebugIcon,
	DeleteIcon,
	EditIcon,
	ForwardIcon,
	MarkAsUnreadIcon,
	PinIcon,
	RemoveAllReactionsIcon,
	ReplyIcon,
	SpeakIcon,
	SuppressEmbedsIcon,
} from '../ContextMenuIcons';
import {MenuItem} from '../MenuItem';

interface MessageMenuItemProps {
	message: MessageRecord;
	onClose: () => void;
}

type AddReactionMenuItemProps = MessageMenuItemProps;

export const AddReactionMenuItem: React.FC<AddReactionMenuItemProps> = observer(({message, onClose}) => {
	const {t} = useLingui();
	const handleAddReaction = React.useCallback(() => {
		triggerAddReaction(message.id);
		onClose();
	}, [message.id, onClose]);

	return (
		<MenuItem icon={<AddReactionIcon />} onClick={handleAddReaction} shortcut="+">
			{t`Add Reaction`}
		</MenuItem>
	);
});

type EditMessageMenuItemProps = MessageMenuItemProps & {
	onEdit: () => void;
};

export const EditMessageMenuItem: React.FC<EditMessageMenuItemProps> = observer(({onEdit, onClose}) => {
	const {t} = useLingui();
	const handleEdit = React.useCallback(() => {
		onEdit();
		onClose();
	}, [onEdit, onClose]);

	return (
		<MenuItem icon={<EditIcon />} onClick={handleEdit} shortcut="e">
			{t`Edit Message`}
		</MenuItem>
	);
});

type ReplyMessageMenuItemProps = MessageMenuItemProps & {
	onReply: () => void;
};

export const ReplyMessageMenuItem: React.FC<ReplyMessageMenuItemProps> = observer(({onReply, onClose}) => {
	const {t} = useLingui();
	const handleReply = React.useCallback(() => {
		onReply();
		onClose();
	}, [onReply, onClose]);

	return (
		<MenuItem icon={<ReplyIcon />} onClick={handleReply} shortcut="r">
			{t`Reply`}
		</MenuItem>
	);
});

type ForwardMessageMenuItemProps = MessageMenuItemProps & {
	onForward: () => void;
};

export const ForwardMessageMenuItem: React.FC<ForwardMessageMenuItemProps> = observer(({onForward, onClose}) => {
	const {t} = useLingui();
	const handleForward = React.useCallback(() => {
		onForward();
		onClose();
	}, [onForward, onClose]);

	return (
		<MenuItem icon={<ForwardIcon />} onClick={handleForward} shortcut="f">
			{t`Forward`}
		</MenuItem>
	);
});

type BookmarkMessageMenuItemProps = MessageMenuItemProps & {
	onSave: (isSaved: boolean) => () => void;
};

export const BookmarkMessageMenuItem: React.FC<BookmarkMessageMenuItemProps> = observer(
	({message, onSave, onClose}) => {
		const {t} = useLingui();
		const isSaved = SavedMessagesStore.isSaved(message.id);

		const handleSave = React.useCallback(() => {
			onSave(isSaved)();
			onClose();
		}, [isSaved, onSave, onClose]);

		return (
			<MenuItem icon={<BookmarkIcon filled={isSaved} />} onClick={handleSave} shortcut="b">
				{isSaved ? t`Remove Bookmark` : t`Bookmark Message`}
			</MenuItem>
		);
	},
);

type PinMessageMenuItemProps = MessageMenuItemProps & {
	onPin: () => void;
};

export const PinMessageMenuItem: React.FC<PinMessageMenuItemProps> = observer(({message, onPin, onClose}) => {
	const {t} = useLingui();
	const handlePin = React.useCallback(() => {
		onPin();
		onClose();
	}, [onPin, onClose]);

	return (
		<MenuItem icon={<PinIcon />} onClick={handlePin} shortcut="p">
			{message.pinned ? t`Unpin Message` : t`Pin Message`}
		</MenuItem>
	);
});

type SuppressEmbedsMenuItemProps = MessageMenuItemProps & {
	onToggleSuppressEmbeds: () => void;
};

export const SuppressEmbedsMenuItem: React.FC<SuppressEmbedsMenuItemProps> = observer(
	({message, onToggleSuppressEmbeds, onClose}) => {
		const {t} = useLingui();
		const handleToggle = React.useCallback(() => {
			onToggleSuppressEmbeds();
			onClose();
		}, [onToggleSuppressEmbeds, onClose]);

		return (
			<MenuItem icon={<SuppressEmbedsIcon />} onClick={handleToggle} shortcut="s">
				{isEmbedsSuppressed(message) ? t`Unsuppress Embeds` : t`Suppress Embeds`}
			</MenuItem>
		);
	},
);

type CopyMessageTextMenuItemProps = MessageMenuItemProps & {
	onCopyMessage: () => void;
};

export const CopyMessageTextMenuItem: React.FC<CopyMessageTextMenuItemProps> = observer(({onCopyMessage, onClose}) => {
	const {t} = useLingui();
	const handleCopy = React.useCallback(() => {
		onCopyMessage();
		onClose();
	}, [onCopyMessage, onClose]);

	return (
		<MenuItem icon={<CopyTextIcon />} onClick={handleCopy} shortcut="c">
			{t`Copy Text`}
		</MenuItem>
	);
});

type CopyMessageLinkMenuItemProps = MessageMenuItemProps & {
	onCopyMessageLink: () => void;
};

export const CopyMessageLinkMenuItem: React.FC<CopyMessageLinkMenuItemProps> = observer(
	({onCopyMessageLink, onClose}) => {
		const {t} = useLingui();
		const handleCopyLink = React.useCallback(() => {
			onCopyMessageLink();
			onClose();
		}, [onCopyMessageLink, onClose]);

		return (
			<MenuItem icon={<CopyLinkIcon />} onClick={handleCopyLink} shortcut="l">
				{t`Copy Message Link`}
			</MenuItem>
		);
	},
);

type CopyMessageIdMenuItemProps = MessageMenuItemProps & {
	onCopyMessageId: () => void;
};

export const CopyMessageIdMenuItem: React.FC<CopyMessageIdMenuItemProps> = observer(({onCopyMessageId, onClose}) => {
	const {t} = useLingui();
	const handleCopyId = React.useCallback(() => {
		onCopyMessageId();
		onClose();
	}, [onCopyMessageId, onClose]);

	return (
		<MenuItem icon={<CopyIdIcon />} onClick={handleCopyId}>
			{t`Copy Message ID`}
		</MenuItem>
	);
});

type DebugMessageMenuItemProps = MessageMenuItemProps;

export const DebugMessageMenuItem: React.FC<DebugMessageMenuItemProps> = observer(({message, onClose}) => {
	const {t} = useLingui();
	const handleDebug = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <MessageDebugModal title={t`Message Debug`} message={message} />));
		onClose();
	}, [message, onClose]);

	return (
		<MenuItem icon={<DebugIcon />} onClick={handleDebug}>
			{t`Debug Message`}
		</MenuItem>
	);
});

type DeleteMessageMenuItemProps = MessageMenuItemProps & {
	onDelete: (bypassConfirm?: boolean) => void;
};

export const DeleteMessageMenuItem: React.FC<DeleteMessageMenuItemProps> = observer(({onDelete, onClose}) => {
	const {t} = useLingui();
	const handleDelete = React.useCallback(
		(event?: unknown) => {
			const shiftKey = Boolean((event as {shiftKey?: boolean} | undefined)?.shiftKey);
			onDelete(shiftKey);
			onClose();
		},
		[onDelete, onClose],
	);

	return (
		<MenuItem icon={<DeleteIcon />} onClick={handleDelete} danger shortcut="d">
			{t`Delete Message`}
		</MenuItem>
	);
});

type RemoveAllReactionsMenuItemProps = MessageMenuItemProps & {
	onRemoveAllReactions: () => void;
};

export const RemoveAllReactionsMenuItem: React.FC<RemoveAllReactionsMenuItemProps> = observer(
	({onRemoveAllReactions, onClose}) => {
		const {t} = useLingui();
		const handleRemoveAll = React.useCallback(() => {
			onRemoveAllReactions();
			onClose();
		}, [onRemoveAllReactions, onClose]);

		return (
			<MenuItem icon={<RemoveAllReactionsIcon />} onClick={handleRemoveAll} danger>
				{t`Remove All Reactions`}
			</MenuItem>
		);
	},
);

type MarkAsUnreadMenuItemProps = MessageMenuItemProps & {
	onMarkAsUnread: () => void;
};

export const MarkAsUnreadMenuItem: React.FC<MarkAsUnreadMenuItemProps> = observer(({onMarkAsUnread, onClose}) => {
	const {t} = useLingui();
	const handleMarkAsUnread = React.useCallback(() => {
		onMarkAsUnread();
		onClose();
	}, [onMarkAsUnread, onClose]);

	return (
		<MenuItem icon={<MarkAsUnreadIcon />} onClick={handleMarkAsUnread} shortcut="u">
			{t`Mark as Unread`}
		</MenuItem>
	);
});

type SpeakMessageMenuItemProps = MessageMenuItemProps;

export const SpeakMessageMenuItem: React.FC<SpeakMessageMenuItemProps> = observer(({message, onClose}) => {
	const {t} = useLingui();
	const handleSpeak = React.useCallback(() => {
		TtsUtils.speakMessage(message.content);
		onClose();
	}, [message.content, onClose]);

	if (!TtsUtils.isSupported()) {
		return null;
	}

	if (!message.content.trim()) {
		return null;
	}

	return (
		<MenuItem icon={<SpeakIcon />} onClick={handleSpeak}>
			{TtsUtils.isSpeaking() ? t`Stop Speaking` : t`Speak Message`}
		</MenuItem>
	);
});
