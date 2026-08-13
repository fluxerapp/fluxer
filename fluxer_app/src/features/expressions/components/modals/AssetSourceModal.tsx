// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import styles from '@app/features/expressions/components/modals/AssetSourceModal.module.css';
import {GifPickerSelectModal} from '@app/features/expressions/components/modals/GifPickerSelectModal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GifIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const UPLOAD_A_FILE_DESCRIPTOR = msg({
	message: 'Upload a file',
	comment: 'Card label in the asset source modal for uploading an image from the device. Keep it concise.',
});
const SEARCH_AND_PICK_A_GIF_DESCRIPTOR = msg({
	message: 'Search GIFs and pick your favorite',
	comment: 'Hint text under the GIF provider card in the asset source modal. Keep it concise.',
});
const CHOOSE_A_GIF_DESCRIPTOR = msg({
	message: 'Choose a GIF',
	comment: 'Title of the GIF picker modal opened from the asset source modal. Keep it concise.',
});

export interface AssetSourceModalProps {
	title: string;
	uploadHint: React.ReactNode;
	onPickUpload: () => void | Promise<void>;
	onSelectGif?: (gif: Gif) => void;
	showGifOption?: boolean;
}

export const AssetSourceModal = observer(function AssetSourceModal({
	title,
	uploadHint,
	onPickUpload,
	onSelectGif,
	showGifOption = true,
}: AssetSourceModalProps) {
	const {i18n} = useLingui();
	const gifProviderName = RuntimeConfig.gifProviderDisplayName;
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handlePickUpload = useCallback(() => {
		const sourceModalKey = ModalCommands.getTopModalKey();
		const closeSourceModal = () => {
			if (sourceModalKey) ModalCommands.popWithKey(sourceModalKey);
		};
		try {
			void Promise.resolve(onPickUpload()).finally(closeSourceModal);
		} catch (error) {
			closeSourceModal();
			throw error;
		}
	}, [onPickUpload]);
	const handlePickGif = useCallback(() => {
		if (!onSelectGif) return;
		ModalCommands.pop();
		ModalCommands.push(
			modal(() => (
				<GifPickerSelectModal
					title={i18n._(CHOOSE_A_GIF_DESCRIPTOR)}
					onSelect={onSelectGif}
					data-flx="expressions.asset-source-modal.handle-pick-gif.gif-picker-select-modal"
				/>
			)),
		);
	}, [i18n, onSelectGif]);
	return (
		<Modal.Root size="small" data-flx="expressions.asset-source-modal.modal-root">
			<Modal.Header title={title} onClose={handleClose} data-flx="expressions.asset-source-modal.modal-header" />
			<Modal.Content data-flx="expressions.asset-source-modal.modal-content">
				<div
					className={`${styles.cards} ${showGifOption ? '' : styles.cardsSingle}`}
					data-flx="expressions.asset-source-modal.cards"
				>
					<FocusRing offset={-2} data-flx="expressions.asset-source-modal.focus-ring">
						<button
							type="button"
							className={styles.card}
							onClick={handlePickUpload}
							data-flx="expressions.asset-source-modal.card.pick-upload"
						>
							<div className={styles.cardIcon} data-flx="expressions.asset-source-modal.card-icon">
								<UploadSimpleIcon
									size={remFromPx(24)}
									weight="bold"
									data-flx="expressions.asset-source-modal.upload-icon"
								/>
							</div>
							<div className={styles.cardLabel} data-flx="expressions.asset-source-modal.card-label">
								{i18n._(UPLOAD_A_FILE_DESCRIPTOR)}
							</div>
							<div className={styles.cardHint} data-flx="expressions.asset-source-modal.card-hint">
								{uploadHint}
							</div>
						</button>
					</FocusRing>
					{showGifOption ? (
						<FocusRing offset={-2} data-flx="expressions.asset-source-modal.focus-ring--2">
							<button
								type="button"
								className={styles.card}
								onClick={handlePickGif}
								data-flx="expressions.asset-source-modal.card.pick-gif"
							>
								<div className={styles.cardIcon} data-flx="expressions.asset-source-modal.card-icon--2">
									<GifIcon size={remFromPx(24)} weight="bold" data-flx="expressions.asset-source-modal.gif-icon" />
								</div>
								<div className={styles.cardLabel} data-flx="expressions.asset-source-modal.card-label--2">
									{gifProviderName}
								</div>
								<div className={styles.cardHint} data-flx="expressions.asset-source-modal.card-hint--2">
									{i18n._(SEARCH_AND_PICK_A_GIF_DESCRIPTOR)}
								</div>
							</button>
						</FocusRing>
					) : null}
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});

export function openAssetSourceModal(props: AssetSourceModalProps): void {
	ModalCommands.push(
		modal(() => <AssetSourceModal {...props} data-flx="expressions.asset-source-modal.asset-source-modal" />),
	);
}
