// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/expressions/components/modals/sticker_form/StickerPreview.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const DARK_THEME_PREVIEW_DESCRIPTOR = msg({message: '{altText} — dark theme preview'});
const LIGHT_THEME_PREVIEW_DESCRIPTOR = msg({message: '{altText} — light theme preview'});

interface StickerPreviewProps {
	imageUrl: string;
	altText: string;
}

export const StickerPreview = observer(function StickerPreview({imageUrl, altText}: StickerPreviewProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.container} data-flx="expressions.sticker-form.sticker-preview.container">
			<div className={styles.title} data-flx="expressions.sticker-form.sticker-preview.title">
				<Trans>Preview</Trans>
			</div>
			<div className={styles.previewContainer} data-flx="expressions.sticker-form.sticker-preview.preview-container">
				<div className={styles.previewItem} data-flx="expressions.sticker-form.sticker-preview.preview-item">
					<div
						className={`${styles.previewBox} ${styles.darkBackground}`}
						data-flx="expressions.sticker-form.sticker-preview.preview-box"
					>
						<img
							src={imageUrl}
							alt={i18n._(DARK_THEME_PREVIEW_DESCRIPTOR, {altText})}
							className={styles.previewImage}
							data-flx="expressions.sticker-form.sticker-preview.preview-image"
						/>
					</div>
					<span className={styles.label} data-flx="expressions.sticker-form.sticker-preview.label">
						<Trans>Dark</Trans>
					</span>
				</div>
				<div className={styles.previewItem} data-flx="expressions.sticker-form.sticker-preview.preview-item--2">
					<div
						className={`${styles.previewBox} ${styles.lightBackground}`}
						data-flx="expressions.sticker-form.sticker-preview.preview-box--2"
					>
						<img
							src={imageUrl}
							alt={i18n._(LIGHT_THEME_PREVIEW_DESCRIPTOR, {altText})}
							className={styles.previewImage}
							data-flx="expressions.sticker-form.sticker-preview.preview-image--2"
						/>
					</div>
					<span className={styles.label} data-flx="expressions.sticker-form.sticker-preview.label--2">
						<Trans>Light</Trans>
					</span>
				</div>
			</div>
		</div>
	);
});
