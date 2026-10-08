// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji, UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import styles from '@app/features/forum/components/Forum.module.css';
import type {ForumEmojiRef} from '@app/features/forum/utils/ForumChannelUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {toReactionEmoji, useEmojiURL} from '@app/features/messaging/utils/ReactionUtils';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

export function forumEmojiFromPicker(emoji: FlatEmoji): ForumEmojiRef {
	const reaction = toReactionEmoji(emoji as UnicodeEmoji);
	return reaction.id ? {emoji_id: reaction.id, emoji_name: null} : {emoji_id: null, emoji_name: reaction.name};
}

export const ForumEmoji = observer(({emoji, className}: {emoji: ForumEmojiRef; className?: string}) => {
	const url = useEmojiURL({
		emoji: {id: emoji.emoji_id, name: emoji.emoji_name ?? 'emoji'},
		size: 32,
	});
	if (!url || (emoji.emoji_id == null && emoji.emoji_name == null)) return null;
	return (
		<img
			src={url}
			alt={emoji.emoji_name ?? ''}
			className={clsx(styles.emoji, className)}
			draggable={false}
			data-flx="forum.forum-tag-pill.forum-emoji.emoji"
		/>
	);
});

interface ForumTagPillProps {
	tag: {id: string; name: string; moderated: boolean} & ForumEmojiRef;
	selected?: boolean;
	disabled?: boolean;
	onClick?: () => void;
}

export const ForumTagPill = observer(({tag, selected = false, disabled = false, onClick}: ForumTagPillProps) => {
	const {i18n} = useLingui();
	const content = (
		<>
			<ForumEmoji emoji={tag} data-flx="forum.forum-tag-pill.forum-emoji" />
			<span className={styles.tagName} data-flx="forum.forum-tag-pill.tag-name">
				{tag.name}
			</span>
		</>
	);
	if (!onClick) {
		return (
			<span className={styles.tag} data-flx="forum.forum-tag-pill.tag">
				{content}
			</span>
		);
	}
	const button = (
		<FocusRing offset={-2} data-flx="forum.forum-tag-pill.focus-ring">
			<button
				type="button"
				className={clsx(styles.tag, styles.tagButton, selected && styles.tagSelected)}
				aria-pressed={selected}
				disabled={disabled}
				onClick={onClick}
				data-flx="forum.forum-tag-pill.tag.click.button"
			>
				{content}
			</button>
		</FocusRing>
	);
	if (!tag.moderated) return button;
	return (
		<Tooltip text={i18n._(D.MODERATED_TAG_HINT_DESCRIPTOR)} data-flx="forum.forum-tag-pill.tooltip">
			{button}
		</Tooltip>
	);
});
