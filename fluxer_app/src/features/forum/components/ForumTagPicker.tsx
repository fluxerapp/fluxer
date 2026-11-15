// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumTagPill} from '@app/features/forum/components/ForumTagPill';
import {getForumTags, isTagRequired} from '@app/features/forum/utils/ForumChannelUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {canUseTag} from '@app/features/forum/utils/ForumPermissions';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {MAX_APPLIED_TAGS_PER_THREAD} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {TagIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useRef, useState} from 'react';

interface ForumTagPickerProps {
	forum: Channel;
	selected: ReadonlyArray<string>;
	onChange: (tagIds: Array<string>) => void;
}

export const ForumTagPicker = observer(({forum, selected, onChange}: ForumTagPickerProps) => {
	const {i18n} = useLingui();
	const full = selected.length >= MAX_APPLIED_TAGS_PER_THREAD;
	return (
		<div className={styles.tagPicker} data-flx="forum.forum-tag-picker.tag-picker">
			<div className={styles.tagRow} data-flx="forum.forum-tag-picker.tag-row">
				{forum.availableTags.map((tag) => {
					const isSelected = selected.includes(tag.id);
					return (
						<ForumTagPill
							key={tag.id}
							tag={tag}
							selected={isSelected}
							disabled={!canUseTag(forum, tag) || (!isSelected && full)}
							onClick={() => onChange(isSelected ? selected.filter((id) => id !== tag.id) : [...selected, tag.id])}
							data-flx="forum.forum-tag-picker.forum-tag-pill.change"
						/>
					);
				})}
			</div>
			<span className={styles.hint} data-flx="forum.forum-tag-picker.hint">
				{i18n._(D.TAG_LIMIT_DESCRIPTOR, {count: MAX_APPLIED_TAGS_PER_THREAD})}
			</span>
		</div>
	);
});

const ForumTagPopoutContent = observer(
	({
		forum,
		initial,
		onChange,
	}: {
		forum: Channel;
		initial: ReadonlyArray<string>;
		onChange: (tagIds: Array<string>) => void;
	}) => {
		const [selected, setSelected] = useState<ReadonlyArray<string>>(initial);
		return (
			<div className={styles.tagPopout} data-flx="forum.forum-composer-tags.tag-popout">
				<ForumTagPicker
					forum={forum}
					selected={selected}
					onChange={(next) => {
						setSelected(next);
						onChange(next);
					}}
					data-flx="forum.forum-composer-tags.forum-tag-picker"
				/>
			</div>
		);
	},
);

export const ForumComposerTags = observer(({forum, selected, onChange}: ForumTagPickerProps) => {
	const {i18n} = useLingui();
	const chosen = getForumTags(forum, selected);
	const latest = useRef({selected, onChange});
	latest.current = {selected, onChange};
	return (
		<div className={styles.composerTags} data-flx="forum.forum-composer-tags.composer-tags">
			<Popout
				position="top-start"
				render={() => (
					<ForumTagPopoutContent
						forum={forum}
						initial={latest.current.selected}
						onChange={(next) => latest.current.onChange(next)}
						data-flx="forum.forum-composer-tags.forum-tag-popout-content"
					/>
				)}
				data-flx="forum.forum-composer-tags.popout"
			>
				<Button
					variant="secondary"
					small
					fitContent
					leftIcon={<TagIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-composer-tags.tag-icon" />}
					aria-haspopup="dialog"
					data-flx="forum.forum-composer-tags.button.add-tags"
				>
					{i18n._(D.ADD_TAGS_DESCRIPTOR)}
				</Button>
			</Popout>
			{chosen.map((tag) => (
				<ForumTagPill
					key={tag.id}
					tag={tag}
					selected
					onClick={() => onChange(selected.filter((id) => id !== tag.id))}
					data-flx="forum.forum-composer-tags.forum-tag-pill.remove"
				/>
			))}
			{isTagRequired(forum) && chosen.length === 0 && (
				<span className={styles.hint} data-flx="forum.forum-composer-tags.required">
					{i18n._(D.TAGS_REQUIRED_DESCRIPTOR)}
				</span>
			)}
		</div>
	);
});
