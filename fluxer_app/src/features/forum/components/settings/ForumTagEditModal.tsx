// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import Channels from '@app/features/channel/state/Channels';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import * as ForumCommands from '@app/features/forum/commands/ForumCommands';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumEmoji, forumEmojiFromPicker} from '@app/features/forum/components/ForumTagPill';
import type {ForumEmojiRef} from '@app/features/forum/utils/ForumChannelUtils';
import {reportForumError} from '@app/features/forum/utils/ForumErrors';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {FORUM_TAG_NAME_MAX_LENGTH} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

export const ForumTagEditModal = observer(({forumId, tagId}: {forumId: string; tagId: string | null}) => {
	const {i18n} = useLingui();
	const forum = Channels.getChannel(forumId);
	const existing = tagId ? forum?.availableTags.find((tag) => tag.id === tagId) : undefined;
	const [name, setName] = useState(existing?.name ?? '');
	const [moderated, setModerated] = useState(existing?.moderated ?? false);
	const [emoji, setEmoji] = useState<ForumEmojiRef>({
		emoji_id: existing?.emoji_id ?? null,
		emoji_name: existing?.emoji_name ?? null,
	});
	const [busy, setBusy] = useState<'save' | 'delete' | null>(null);
	if (!forum || (tagId != null && !existing)) return null;
	const run = async (kind: 'save' | 'delete', action: () => Promise<void>) => {
		setBusy(kind);
		try {
			await action();
			ModalCommands.pop();
		} catch (error) {
			reportForumError(i18n, error, D.TAG_SAVE_FAILED_DESCRIPTOR);
		} finally {
			setBusy(null);
		}
	};
	const input = {name: name.trim(), moderated, ...emoji};
	const handleSave = () =>
		run('save', () =>
			tagId ? ForumCommands.updateForumTag(forum, tagId, input) : ForumCommands.createForumTag(forum, input),
		);
	const hasEmoji = emoji.emoji_id != null || emoji.emoji_name != null;
	return (
		<Modal.Root size="small" centered data-flx="forum.settings.forum-tag-edit-modal.modal-root">
			<Modal.Header
				title={i18n._(tagId ? D.EDIT_TAG_DESCRIPTOR : D.CREATE_TAG_DESCRIPTOR)}
				data-flx="forum.settings.forum-tag-edit-modal.modal-header"
			/>
			<Modal.Content data-flx="forum.settings.forum-tag-edit-modal.modal-content">
				<div className={styles.settingsSection} data-flx="forum.settings.forum-tag-edit-modal.settings-section">
					<Input
						value={name}
						onChange={(event) => setName(event.target.value)}
						label={i18n._(D.TAG_NAME_DESCRIPTOR)}
						maxLength={FORUM_TAG_NAME_MAX_LENGTH}
						autoFocus
						autoComplete="off"
						data-flx="forum.settings.forum-tag-edit-modal.input.set-name"
					/>
					<span className={styles.fieldLabel} data-flx="forum.settings.forum-tag-edit-modal.settings-label">
						{i18n._(D.TAG_EMOJI_DESCRIPTOR)}
					</span>
					<div className={styles.settingsRow} data-flx="forum.settings.forum-tag-edit-modal.settings-row">
						<Popout
							position="right-start"
							render={({onClose}) => (
								<EmojiPickerPopout
									channelId={forum.id}
									handleSelect={(picked) => setEmoji(forumEmojiFromPicker(picked))}
									onClose={onClose}
									data-flx="forum.settings.forum-tag-edit-modal.emoji-picker-popout"
								/>
							)}
							data-flx="forum.settings.forum-tag-edit-modal.popout"
						>
							<FocusRing offset={-2} data-flx="forum.settings.forum-tag-edit-modal.focus-ring">
								<button
									type="button"
									className={styles.emojiPickerButton}
									aria-label={i18n._(D.PICK_EMOJI_DESCRIPTOR)}
									data-flx="forum.settings.forum-tag-edit-modal.emoji-picker-button"
								>
									{hasEmoji ? (
										<ForumEmoji emoji={emoji} data-flx="forum.settings.forum-tag-edit-modal.forum-emoji" />
									) : (
										<SmileyIcon size={20} data-flx="forum.settings.forum-tag-edit-modal.smiley-icon" />
									)}
								</button>
							</FocusRing>
						</Popout>
						{hasEmoji && (
							<Button
								small
								variant="secondary"
								onClick={() => setEmoji({emoji_id: null, emoji_name: null})}
								data-flx="forum.settings.forum-tag-edit-modal.button.set-emoji"
							>
								{i18n._(D.REMOVE_DESCRIPTOR)}
							</Button>
						)}
					</div>
					<Switch
						label={i18n._(D.TAG_MODERATED_DESCRIPTOR)}
						description={i18n._(D.TAG_MODERATED_HINT_DESCRIPTOR)}
						value={moderated}
						onChange={setModerated}
						data-flx="forum.settings.forum-tag-edit-modal.switch.set-moderated"
					/>
				</div>
			</Modal.Content>
			<Modal.Footer data-flx="forum.settings.forum-tag-edit-modal.modal-footer">
				{tagId && (
					<Button
						variant="danger"
						submitting={busy === 'delete'}
						disabled={busy != null}
						onClick={() => run('delete', () => ForumCommands.deleteForumTag(forum, tagId))}
						data-flx="forum.settings.forum-tag-edit-modal.button.run"
					>
						{i18n._(D.DELETE_TAG_DESCRIPTOR)}
					</Button>
				)}
				<Button
					onClick={ModalCommands.pop}
					variant="secondary"
					data-flx="forum.settings.forum-tag-edit-modal.button.pop"
				>
					{i18n._(D.CANCEL_DESCRIPTOR)}
				</Button>
				<Button
					onClick={handleSave}
					submitting={busy === 'save'}
					disabled={busy != null || input.name.length === 0}
					data-flx="forum.settings.forum-tag-edit-modal.button.save"
				>
					{i18n._(D.SAVE_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
