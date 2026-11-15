// SPDX-License-Identifier: AGPL-3.0-or-later

import {canReportMessage} from '@app/features/channel/components/MessageActionUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ForumCommands from '@app/features/forum/commands/ForumCommands';
import {ForumPostTagsModal} from '@app/features/forum/components/ForumPostTagsModal';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import {reportForumError} from '@app/features/forum/utils/ForumErrors';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {canEditPostTags} from '@app/features/forum/utils/ForumPermissions';
import {isPinnedPost} from '@app/features/forum/utils/ForumPostList';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {openReportMessageModal} from '@app/features/moderation/utils/ReportActionUtils';
import {useThreadMenuData} from '@app/features/threads/hooks/useThreadMenuData';
import {canPatchThread, isThreadModeratorFor} from '@app/features/threads/utils/ThreadActionRules';
import {ReportMessageIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {ChannelFlags} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {PushPinSimpleIcon, PushPinSimpleSlashIcon, TagIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

function openForumPostTagsModal(post: Channel): void {
	ModalCommands.push(
		modal(() => (
			<ForumPostTagsModal
				postId={post.id}
				data-flx="forum.forum-post-context-menu.open-forum-post-tags-modal.forum-post-tags-modal"
			/>
		)),
	);
}

function useForumPostMenuData(post: Channel, onClose: () => void): Array<MenuGroupType> {
	const {i18n} = useLingui();
	const {groups} = useThreadMenuData(post, {onClose});
	const firstMessage = ForumPosts.getFirstMessage(post.id);
	const reportableMessage = useMemo(() => {
		if (!firstMessage) return null;
		const message = new Message(firstMessage, {skipReactionHydration: true});
		return canReportMessage(message) ? message : null;
	}, [firstMessage]);
	const forum = getPostForum(post);
	if (!forum) return groups;
	const pinned = isPinnedPost(post);
	const items: MenuGroupType['items'] = [];
	const moderator = isThreadModeratorFor(post);
	const nextFlags = pinned ? post.flags & ~ChannelFlags.PINNED : post.flags | ChannelFlags.PINNED;
	if (moderator && !post.isArchived && canPatchThread(post, {flags: nextFlags})) {
		items.push({
			icon: pinned ? (
				<PushPinSimpleSlashIcon
					size={20}
					data-flx="forum.forum-post-context-menu.use-forum-post-menu-data.push-pin-simple-slash-icon"
				/>
			) : (
				<PushPinSimpleIcon
					size={20}
					data-flx="forum.forum-post-context-menu.use-forum-post-menu-data.push-pin-simple-icon"
				/>
			),
			label: pinned ? i18n._(D.UNPIN_POST_DESCRIPTOR) : i18n._(D.PIN_POST_DESCRIPTOR),
			onClick: () => {
				void ForumCommands.setPostPinned(post, !pinned).catch((error) => reportForumError(i18n, error));
				onClose();
			},
		});
	}
	if (forum.availableTags.length > 0 && canEditPostTags(post)) {
		items.push({
			icon: <TagIcon size={20} data-flx="forum.forum-post-context-menu.use-forum-post-menu-data.tag-icon" />,
			label: i18n._(D.EDIT_TAGS_DESCRIPTOR),
			onClick: () => {
				openForumPostTagsModal(post);
				onClose();
			},
		});
	}
	const reportItems: MenuGroupType['items'] = [];
	if (reportableMessage) {
		reportItems.push({
			icon: (
				<ReportMessageIcon
					size={20}
					data-flx="forum.forum-post-context-menu.use-forum-post-menu-data.report-message-icon"
				/>
			),
			label: i18n._(D.REPORT_POST_DESCRIPTOR),
			onClick: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => openReportMessageModal(reportableMessage));
			},
			danger: true,
		});
	}
	const postGroups = items.length > 0 ? [{items}, ...groups] : groups;
	if (reportItems.length === 0) return postGroups;
	if (postGroups.length === 0) return [{items: reportItems}];
	return [...postGroups.slice(0, -1), {items: reportItems}, postGroups[postGroups.length - 1]];
}

const ForumPostContextMenu = observer(({post, onClose}: {post: Channel; onClose: () => void}) => {
	const groups = useForumPostMenuData(post, onClose);
	return <DataMenuRenderer groups={groups} data-flx="forum.forum-post-context-menu.data-menu-renderer" />;
});

export const ForumPostMenuSheet = observer(({post, onClose}: {post: Channel; onClose: () => void}) => {
	const groups = useForumPostMenuData(post, onClose);
	return (
		<MenuBottomSheet
			isOpen={true}
			onClose={onClose}
			title={post.name ?? undefined}
			groups={groups}
			data-flx="forum.forum-post-context-menu.forum-post-menu-sheet.menu-bottom-sheet"
		/>
	);
});

export function openForumPostContextMenu(event: React.MouseEvent, post: Channel): void {
	ContextMenuCommands.openFromEvent(event, ({onClose}) => (
		<ForumPostContextMenu
			post={post}
			onClose={onClose}
			data-flx="forum.forum-post-context-menu.open-forum-post-context-menu.forum-post-context-menu"
		/>
	));
}
