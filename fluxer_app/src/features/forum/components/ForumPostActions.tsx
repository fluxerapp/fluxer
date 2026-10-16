// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import type {UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import styles from '@app/features/forum/components/ForumPostActions.module.css';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {buildChannelLink} from '@app/features/messaging/utils/MessageLinkUtils';
import {toReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {reportThreadActionError} from '@app/features/threads/hooks/useThreadMenuData';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {canJoinThreadChannel} from '@app/features/threads/utils/ThreadActionRules';
import {Button} from '@app/features/ui/button/Button';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {BellIcon, CheckIcon, LinkSimpleIcon, SmileyIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

export const ForumPostActions = observer(({post, messageId}: {post: Channel; messageId: string}) => {
	const {i18n} = useLingui();
	const [busy, setBusy] = useState(false);
	const following = ThreadMemberships.isMember(post.id);
	const canReact = Permission.can(Permissions.ADD_REACTIONS, post) && !post.isLocked && !post.isArchived;
	const canFollow = following || canJoinThreadChannel(post);
	const toggleFollow = () => {
		setBusy(true);
		void (following ? ThreadCommands.leaveThread(post) : ThreadCommands.joinThread(post))
			.catch((error) => reportThreadActionError(i18n, error))
			.finally(() => setBusy(false));
	};
	const copyLink = () => {
		void TextCopyCommands.copy(i18n, buildChannelLink({guildId: post.guildId, channelId: post.id}), true);
		ToastCommands.createToast({type: 'success', children: i18n._(D.POST_LINK_COPIED_DESCRIPTOR)});
	};
	return (
		<div className={styles.actions} data-flx="forum.forum-post-actions.actions">
			{canReact && (
				<Popout
					position="top-start"
					render={({onClose}) => (
						<EmojiPickerPopout
							channelId={post.id}
							handleSelect={(emoji) => {
								ReactionCommands.addReaction(i18n, post.id, messageId, toReactionEmoji(emoji as UnicodeEmoji));
								onClose();
							}}
							onClose={onClose}
							data-flx="forum.forum-post-actions.emoji-picker-popout"
						/>
					)}
					data-flx="forum.forum-post-actions.react-popout"
				>
					<Button
						variant="secondary"
						small
						fitContent
						leftIcon={<SmileyIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-post-actions.smiley-icon" />}
						data-flx="forum.forum-post-actions.button.react"
					>
						{i18n._(D.REACT_TO_POST_DESCRIPTOR)}
					</Button>
				</Popout>
			)}
			{canFollow && (
				<Button
					variant="secondary"
					small
					fitContent
					submitting={busy}
					aria-pressed={following}
					leftIcon={
						following ? (
							<CheckIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-post-actions.check-icon" />
						) : (
							<BellIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-post-actions.bell-icon" />
						)
					}
					onClick={toggleFollow}
					data-flx="forum.forum-post-actions.button.follow"
				>
					{i18n._(following ? D.FOLLOWING_POST_DESCRIPTOR : D.FOLLOW_POST_DESCRIPTOR)}
				</Button>
			)}
			<Tooltip text={i18n._(D.COPY_POST_LINK_DESCRIPTOR)} data-flx="forum.forum-post-actions.copy-tooltip">
				<Button
					variant="secondary"
					small
					square
					icon={<LinkSimpleIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-post-actions.link-icon" />}
					aria-label={i18n._(D.COPY_POST_LINK_DESCRIPTOR)}
					onClick={copyLink}
					data-flx="forum.forum-post-actions.button.copy-link"
				/>
			</Tooltip>
		</div>
	);
});
