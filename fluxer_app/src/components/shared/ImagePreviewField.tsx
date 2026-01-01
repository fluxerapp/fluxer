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

import type React from 'react';
import styles from './ImagePreviewField.module.css';

export interface ImagePreviewFieldProps {
	imageUrl: string | null | undefined;
	showPlaceholder: boolean;
	placeholderText: React.ReactNode;
	altText: string;
	aspectRatio?: string | number;
	className?: string;
	objectFit?: 'cover' | 'contain';
}

export const ImagePreviewField: React.FC<ImagePreviewFieldProps> = ({
	imageUrl,
	showPlaceholder,
	placeholderText,
	altText,
	aspectRatio,
	className,
	objectFit = 'cover',
}) => {
	const innerContainerStyle: React.CSSProperties = aspectRatio
		? {
				position: 'relative',
				width: '100%',
				paddingBottom: `${(1 / Number(aspectRatio)) * 100}%`,
			}
		: {};

	const imageContainerStyle: React.CSSProperties = aspectRatio
		? {
				position: 'absolute',
				top: 0,
				left: 0,
				width: '100%',
				height: '100%',
			}
		: {};

	const imageStyle: React.CSSProperties = {
		objectFit,
		width: '100%',
		height: '100%',
	};

	if (showPlaceholder || !imageUrl) {
		return (
			<div className={`${styles.placeholder} ${className ?? ''}`} style={aspectRatio ? innerContainerStyle : {}}>
				{aspectRatio ? (
					<div style={imageContainerStyle}>
						<span>{placeholderText}</span>
					</div>
				) : (
					<span>{placeholderText}</span>
				)}
			</div>
		);
	}

	return (
		<div className={`${styles.preview} ${className ?? ''}`} style={aspectRatio ? innerContainerStyle : {}}>
			{aspectRatio ? (
				<div style={imageContainerStyle}>
					<img src={imageUrl} alt={altText} className={styles.image} style={imageStyle} />
				</div>
			) : (
				<img src={imageUrl} alt={altText} className={styles.image} style={imageStyle} />
			)}
		</div>
	);
};
