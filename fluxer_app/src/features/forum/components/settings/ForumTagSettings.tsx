// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import type {Channel} from '@app/features/channel/models/Channel';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumEmoji} from '@app/features/forum/components/ForumTagPill';
import {ForumTagEditModal} from '@app/features/forum/components/settings/ForumTagEditModal';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {MAX_FORUM_TAGS_PER_CHANNEL} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {LockSimpleIcon, PencilSimpleIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

function openTagEditor(forumId: string, tagId: string | null): void {
	ModalCommands.push(
		modal(() => (
			<ForumTagEditModal
				forumId={forumId}
				tagId={tagId}
				data-flx="forum.settings.forum-tag-settings.open-tag-editor.forum-tag-edit-modal"
			/>
		)),
	);
}

export const ForumTagSettings = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const tags = forum.availableTags;
	return (
		<SettingsControlRow
			label={i18n._(D.TAGS_DESCRIPTOR)}
			description={i18n._(D.TAGS_HINT_DESCRIPTOR, {count: MAX_FORUM_TAGS_PER_CHANNEL})}
			stacked
			dataFlx="forum.settings.forum-tag-settings"
			data-flx="forum.settings.forum-tag-settings.settings-control-row"
		>
			{tags.length === 0 ? (
				<span className={styles.hint} data-flx="forum.settings.forum-tag-settings.hint--2">
					{i18n._(D.NO_TAGS_DESCRIPTOR)}
				</span>
			) : (
				<div className={styles.tagList} data-flx="forum.settings.forum-tag-settings.tag-list">
					{tags.map((tag) => (
						<div key={tag.id} className={styles.tagListItem} data-flx="forum.settings.forum-tag-settings.tag-list-item">
							<span className={styles.tagListEmoji} data-flx="forum.settings.forum-tag-settings.tag-list-emoji">
								<ForumEmoji emoji={tag} data-flx="forum.settings.forum-tag-settings.forum-emoji" />
							</span>
							<span className={styles.tagListName} data-flx="forum.settings.forum-tag-settings.tag-list-name">
								{tag.name}
							</span>
							{tag.moderated && (
								<LockSimpleIcon
									size={14}
									aria-label={i18n._(D.TAG_MODERATED_DESCRIPTOR)}
									data-flx="forum.settings.forum-tag-settings.lock-simple-icon"
								/>
							)}
							<FocusRing offset={-2} data-flx="forum.settings.forum-tag-settings.focus-ring">
								<button
									type="button"
									className={styles.iconButton}
									onClick={() => openTagEditor(forum.id, tag.id)}
									aria-label={i18n._(D.EDIT_TAG_DESCRIPTOR)}
									data-flx="forum.settings.forum-tag-settings.icon-button.open-tag-editor"
								>
									<PencilSimpleIcon size={16} data-flx="forum.settings.forum-tag-settings.pencil-simple-icon" />
								</button>
							</FocusRing>
						</div>
					))}
				</div>
			)}
			<div className={styles.settingsRow} data-flx="forum.settings.forum-tag-settings.settings-row">
				<Button
					small
					variant="secondary"
					leftIcon={<PlusIcon size={16} weight="bold" data-flx="forum.settings.forum-tag-settings.plus-icon" />}
					disabled={tags.length >= MAX_FORUM_TAGS_PER_CHANNEL}
					onClick={() => openTagEditor(forum.id, null)}
					data-flx="forum.settings.forum-tag-settings.button.open-tag-editor"
				>
					{i18n._(D.ADD_TAG_DESCRIPTOR)}
				</Button>
			</div>
		</SettingsControlRow>
	);
});
