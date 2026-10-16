// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_GIF_URLS} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/channel/components/pickers/gif/ImportFavoriteGifsModal.module.css';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import FavoriteGifImport from '@app/features/expressions/state/FavoriteGifImport';
import {FAVORITE_GIF_LIMIT_REACHED_DESCRIPTOR} from '@app/features/expressions/utils/FavoriteGifMessageDescriptors';
import {CANCEL_DESCRIPTOR, CLOSE_DESCRIPTOR, DONE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Textarea} from '@app/features/ui/components/form/FormInput';
import {MAX_FAVORITE_GIFS} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const IMPORT_FAVORITE_GIFS_DESCRIPTOR = msg({
	message: 'Import favorite GIFs',
	comment: 'Short label in the channel and chat import favorite gifs modal. Keep it concise.',
});
const GIF_URLS_DESCRIPTOR = msg({
	message: 'GIF URLs',
	comment: 'Short label in the channel and chat import favorite gifs modal. Keep it concise.',
});
const IMPORT_COMPLETE_WITH_SKIPPED_DESCRIPTOR = msg({
	message:
		'Import complete. Added {importedCount, plural, one {# GIF} other {# GIFs}}, skipped {skippedCount, number} (duplicates or limit reached).',
	comment:
		'Completion text in the favorite GIF import modal. importedCount is the number added; skippedCount is the number skipped.',
});
const IMPORT_COMPLETE_DESCRIPTOR = msg({
	message: 'Import complete. Added {importedCount, plural, one {# GIF} other {# GIFs}}.',
	comment: 'Completion text in the favorite GIF import modal. importedCount is the number added.',
});
const IMPORT_PROGRESS_DESCRIPTOR = msg({
	message: 'Importing… {processedCount, number} / {totalCount, number} processed, {importedCount, number} added.',
	comment:
		'Progress text in the favorite GIF import modal. processedCount is completed URLs, totalCount is queued URLs, importedCount is successfully added GIFs.',
});
const IMPORT_BACKGROUND_CONTINUES_DESCRIPTOR = msg({
	message: 'You can close this modal. The import will continue in the background.',
	comment: 'Informational text in the favorite GIF import modal while import is running.',
});
const PASTE_GIF_URLS_DESCRIPTOR = msg({
	message: 'Paste GIF URLs below, one per line.',
	comment: 'Instruction text in the favorite GIF import modal.',
});
const IMPORT_MORE_DESCRIPTOR = msg({
	message: 'Import more',
	comment: 'Button in the favorite GIF import modal after a completed import.',
});
const CANCEL_IMPORT_DESCRIPTOR = msg({
	message: 'Cancel import',
	comment: 'Danger button in the favorite GIF import modal. Stops the active import.',
});
const IMPORT_DESCRIPTOR = msg({
	message: 'Import',
	comment: 'Primary button in the favorite GIF import modal. Starts importing pasted GIF URLs.',
});
export const ImportFavoriteGifsModal = observer(function ImportFavoriteGifsModal() {
	const {i18n} = useLingui();
	const [text, setText] = useState('');
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleImport = useCallback(async () => {
		if (FavoriteGifImport.isRunning) return;
		await FavoriteGifImport.startImport(text);
	}, [text]);
	const handleCancel = useCallback(() => {
		FavoriteGifImport.cancelImport();
	}, []);
	const handleReset = useCallback(() => {
		FavoriteGifImport.reset();
		setText('');
	}, []);
	const isAtLimit = FavoriteGif.totalCount >= MAX_FAVORITE_GIFS;
	return (
		<Modal.Root size="small" data-flx="channel.pickers.gif.import-favorite-gifs-modal.modal-root">
			<Modal.Header
				title={i18n._(IMPORT_FAVORITE_GIFS_DESCRIPTOR)}
				onClose={handleClose}
				data-flx="channel.pickers.gif.import-favorite-gifs-modal.modal-header"
			/>
			<Modal.Content data-flx="channel.pickers.gif.import-favorite-gifs-modal.modal-content">
				{FavoriteGifImport.isDone ? (
					<div className={styles.body} data-flx="channel.pickers.gif.import-favorite-gifs-modal.body">
						<p className={styles.description} data-flx="channel.pickers.gif.import-favorite-gifs-modal.description">
							{FavoriteGifImport.skippedCount > 0
								? i18n._(IMPORT_COMPLETE_WITH_SKIPPED_DESCRIPTOR, {
										importedCount: FavoriteGifImport.importedCount,
										skippedCount: FavoriteGifImport.skippedCount,
									})
								: i18n._(IMPORT_COMPLETE_DESCRIPTOR, {
										importedCount: FavoriteGifImport.importedCount,
									})}
						</p>
					</div>
				) : FavoriteGifImport.isRunning ? (
					<div className={styles.body} data-flx="channel.pickers.gif.import-favorite-gifs-modal.body--2">
						<p className={styles.description} data-flx="channel.pickers.gif.import-favorite-gifs-modal.description--2">
							{i18n._(IMPORT_PROGRESS_DESCRIPTOR, {
								processedCount: FavoriteGifImport.processedCount,
								totalCount: FavoriteGifImport.totalToImport,
								importedCount: FavoriteGifImport.importedCount,
							})}
						</p>
						<div
							className={styles.progressTrack}
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.progress-track"
						>
							<div
								className={styles.progressBar}
								style={{width: `${Math.round(FavoriteGifImport.progress * 100)}%`}}
								data-flx="channel.pickers.gif.import-favorite-gifs-modal.progress-bar"
							/>
						</div>
						<p className={styles.muted} data-flx="channel.pickers.gif.import-favorite-gifs-modal.muted">
							{i18n._(IMPORT_BACKGROUND_CONTINUES_DESCRIPTOR)}
						</p>
					</div>
				) : (
					<div className={styles.body} data-flx="channel.pickers.gif.import-favorite-gifs-modal.body--3">
						<p className={styles.description} data-flx="channel.pickers.gif.import-favorite-gifs-modal.description--3">
							{i18n._(PASTE_GIF_URLS_DESCRIPTOR)}
						</p>
						{isAtLimit ? (
							<p className={styles.danger} data-flx="channel.pickers.gif.import-favorite-gifs-modal.danger">
								{i18n._(FAVORITE_GIF_LIMIT_REACHED_DESCRIPTOR)}
							</p>
						) : (
							<Textarea
								label={i18n._(GIF_URLS_DESCRIPTOR)}
								value={text}
								onChange={(e) => setText(e.target.value)}
								placeholder={EXAMPLE_GIF_URLS}
								minRows={8}
								maxRows={14}
								spellCheck={false}
								autoCapitalize="off"
								autoCorrect="off"
								autoComplete="off"
								data-flx="channel.pickers.gif.import-favorite-gifs-modal.textarea.set-text"
							/>
						)}
					</div>
				)}
			</Modal.Content>
			<Modal.Footer data-flx="channel.pickers.gif.import-favorite-gifs-modal.modal-footer">
				{FavoriteGifImport.isDone ? (
					<>
						<Button
							onClick={handleReset}
							variant="secondary"
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.button.reset"
						>
							{i18n._(IMPORT_MORE_DESCRIPTOR)}
						</Button>
						<Button
							onClick={handleClose}
							variant="primary"
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.button.close"
						>
							{i18n._(DONE_DESCRIPTOR)}
						</Button>
					</>
				) : FavoriteGifImport.isRunning ? (
					<>
						<Button
							onClick={handleClose}
							variant="secondary"
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.button.close--2"
						>
							{i18n._(CLOSE_DESCRIPTOR)}
						</Button>
						<Button
							onClick={handleCancel}
							variant="danger"
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.button.cancel"
						>
							{i18n._(CANCEL_IMPORT_DESCRIPTOR)}
						</Button>
					</>
				) : (
					<>
						<Button
							onClick={handleClose}
							variant="secondary"
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.button.close--3"
						>
							{i18n._(CANCEL_DESCRIPTOR)}
						</Button>
						<Button
							onClick={handleImport}
							variant="primary"
							disabled={!text.trim() || isAtLimit}
							data-flx="channel.pickers.gif.import-favorite-gifs-modal.button.import"
						>
							{i18n._(IMPORT_DESCRIPTOR)}
						</Button>
					</>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
