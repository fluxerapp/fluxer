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

import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as UserNoteActionCreators from '~/actions/UserNoteActionCreators';
import styles from '~/components/modals/NoteEditSheet.module.css';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {TextareaAutosize} from '~/lib/TextareaAutosize';

interface NoteEditSheetProps {
	isOpen: boolean;
	onClose: () => void;
	userId: string;
	initialNote: string | null;
}

export const NoteEditSheet: React.FC<NoteEditSheetProps> = observer(({isOpen, onClose, userId, initialNote}) => {
	const {t} = useLingui();
	const userNoteId = React.useId();
	const [note, setNote] = React.useState(initialNote || '');
	const [hasChanges, setHasChanges] = React.useState(false);

	React.useEffect(() => {
		setHasChanges(note !== (initialNote || ''));
	}, [note, initialNote]);

	const handleSave = () => {
		if (hasChanges) {
			UserNoteActionCreators.update(userId, note);
			onClose();
		}
	};

	const saveButton = (
		<button
			type="button"
			onClick={handleSave}
			disabled={!hasChanges}
			className={clsx(styles.saveButton, hasChanges ? styles.saveButtonActive : styles.saveButtonDisabled)}
		>
			<Trans>Save</Trans>
		</button>
	);

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
				<button type="button" onClick={onClose} className={styles.backButton}>
					<ArrowLeftIcon className={styles.backIcon} weight="bold" />
				</button>
			}
			title={t`Edit Note`}
			trailingAction={saveButton}
		>
			<div className={styles.container}>
				<div className={styles.content}>
					<label htmlFor={userNoteId} className={styles.label}>
						<Trans>Note (only visible to you)</Trans>
					</label>
					<TextareaAutosize
						id={userNoteId}
						className={styles.textarea}
						placeholder={t`Tap to add a note`}
						value={note}
						onChange={(e) => setNote(e.target.value)}
						minRows={6}
						maxRows={12}
						maxLength={256}
					/>
				</div>
			</div>
		</BottomSheet>
	);
});
