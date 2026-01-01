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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import styles from './StickerPreview.module.css';

interface StickerPreviewProps {
	imageUrl: string;
	altText: string;
}

export const StickerPreview = observer(function StickerPreview({imageUrl, altText}: StickerPreviewProps) {
	return (
		<div className={styles.container}>
			<div className={styles.title}>
				<Trans>Preview</Trans>
			</div>
			<div className={styles.previewContainer}>
				<div className={styles.previewItem}>
					<div className={`${styles.previewBox} ${styles.darkBackground}`}>
						<img src={imageUrl} alt={`${altText} - Dark theme preview`} className={styles.previewImage} />
					</div>
					<span className={styles.label}>
						<Trans>Dark</Trans>
					</span>
				</div>
				<div className={styles.previewItem}>
					<div className={`${styles.previewBox} ${styles.lightBackground}`}>
						<img src={imageUrl} alt={`${altText} - Light theme preview`} className={styles.previewImage} />
					</div>
					<span className={styles.label}>
						<Trans>Light</Trans>
					</span>
				</div>
			</div>
		</div>
	);
});
