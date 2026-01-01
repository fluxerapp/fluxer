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

import {useLingui} from '@lingui/react/macro';
import {UploadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import styles from './UploadDropZone.module.css';

interface UploadDropZoneProps {
	onDrop: (files: Array<File>) => void;
	description: React.ReactNode;
	acceptMultiple?: boolean;
}

export const UploadDropZone: React.FC<UploadDropZoneProps> = observer(
	({onDrop, description, acceptMultiple = true}) => {
		const {t} = useLingui();
		const [isDragging, setIsDragging] = React.useState(false);

		const handleDragOver = (e: React.DragEvent) => {
			e.preventDefault();
			e.stopPropagation();
			setIsDragging(true);
		};

		const handleDragLeave = (e: React.DragEvent) => {
			e.preventDefault();
			e.stopPropagation();
			setIsDragging(false);
		};

		const handleDrop = (e: React.DragEvent) => {
			e.preventDefault();
			e.stopPropagation();
			setIsDragging(false);

			const files = Array.from(e.dataTransfer.files);
			if (files.length > 0) {
				onDrop(acceptMultiple ? files : [files[0]]);
			}
		};

		return (
			// biome-ignore lint/a11y/noStaticElementInteractions: Drag-and-drop is a progressive enhancement; file upload button available as alternative
			// biome-ignore lint/a11y/useAriaPropsSupportedByRole: aria-label used for descriptive purposes on drop zone
			<div
				aria-label={t`Drag and drop area for file upload`}
				onDragOver={handleDragOver}
				onDragLeave={handleDragLeave}
				onDrop={handleDrop}
				className={`${styles.dropZone} ${isDragging ? styles.dropZoneDragging : ''}`}
			>
				<UploadIcon className={styles.icon} />
				<p className={styles.description}>{description}</p>
			</div>
		);
	},
);
