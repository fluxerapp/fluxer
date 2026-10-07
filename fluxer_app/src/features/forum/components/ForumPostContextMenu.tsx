// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as ForumCommands from '@app/features/forum/commands/ForumCommands';
import {ForumPostTagsModal} from '@app/features/forum/components/ForumPostTagsModal';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import {reportForumError} from '@app/features/forum/utils/ForumErrors';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {canEditPostTags} from '@app/features/forum/utils/ForumPermissions';
import {isPinnedPost} from '@app/features/forum/utils/ForumPostList';
import {useThreadMenuData} from '@app/features/threads/hooks/useThreadMenuData';
import {canPatchThread, isThreadModeratorFor} from '@app/features/threads/utils/ThreadActionRules';
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

export function openForumPostTagsModal(post: Channel): void {
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
	return items.length > 0 ? [{items}, ...groups] : groups;
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
