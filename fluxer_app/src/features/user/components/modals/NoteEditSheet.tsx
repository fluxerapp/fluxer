// SPDX-License-Identifier: AGPL-3.0-or-later

import {GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {TextareaAutosize} from '@app/features/platform/utils/AutoResizingTextarea';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as UserNoteCommands from '@app/features/user/commands/UserNoteCommands';
import styles from '@app/features/user/components/modals/NoteEditSheet.module.css';
import {EDIT_NOTE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId, useState} from 'react';

const TAP_TO_ADD_A_NOTE_DESCRIPTOR = msg({
	message: 'Tap to add a note',
	comment: 'Label in the note edit sheet.',
});

interface NoteEditSheetProps {
	isOpen: boolean;
	onClose: () => void;
	userId: string;
	initialNote: string | null;
}

export const NoteEditSheet: React.FC<NoteEditSheetProps> = observer(({isOpen, onClose, userId, initialNote}) => {
	const {i18n} = useLingui();
	const userNoteId = useId();
	const [note, setNote] = useState(initialNote || '');
	const handleSave = () => {
		UserNoteCommands.update(userId, note);
		onClose();
	};
	const saveButton = (
		<button
			type="button"
			onClick={handleSave}
			className={clsx(styles.saveButton, styles.saveButtonActive)}
			data-flx="user.note-edit-sheet.save-button"
		>
			<Trans>Save</Trans>
		</button>
	);
	if (StreamerMode.shouldHidePersonalInformation) {
		return null;
	}
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			disablePadding={true}
			surface="primary"
			showCloseButton={false}
			leadingAction={
				<button
					type="button"
					onClick={onClose}
					className={styles.backButton}
					aria-label={i18n._(GO_BACK_DESCRIPTOR)}
					data-flx="user.note-edit-sheet.back-button.close"
				>
					<ArrowLeftIcon className={styles.backIcon} weight="bold" data-flx="user.note-edit-sheet.back-icon" />
				</button>
			}
			title={i18n._(EDIT_NOTE_DESCRIPTOR)}
			trailingAction={saveButton}
			data-flx="user.note-edit-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="user.note-edit-sheet.container">
				<div className={styles.content} data-flx="user.note-edit-sheet.content">
					<label htmlFor={userNoteId} className={styles.label} data-flx="user.note-edit-sheet.label">
						<Trans>Note (only visible to you)</Trans>
					</label>
					<TextareaAutosize
						id={userNoteId}
						className={styles.textarea}
						placeholder={i18n._(TAP_TO_ADD_A_NOTE_DESCRIPTOR)}
						value={note}
						onChange={(e) => setNote(e.target.value)}
						minRows={6}
						maxRows={12}
						maxLength={256}
						data-flx="user.note-edit-sheet.textarea.set-note"
					/>
				</div>
			</div>
		</BottomSheet>
	);
});
