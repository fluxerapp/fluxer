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
import {MusicNoteIcon, PencilSimpleIcon, TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import gifStyles from '~/components/channel/GifPicker.module.css';
import {useGifVideoPool} from '~/components/channel/GifVideoPool';
import styles from '~/components/channel/MemesPicker.module.css';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {EditFavoriteMemeModal} from '~/components/modals/EditFavoriteMemeModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';
import MemesPickerStore from '~/stores/MemesPickerStore';
import {usePooledVideo} from '../shared/usePooledVideo';
import {formatDuration, getFileExtension} from './mediaFormat';

const GifIndicator = observer(() => (
	<div className={styles.gifBadge} aria-hidden="true">
		GIF
	</div>
));

export const MemeGridItem = observer(
	({
		meme,
		coords,
		onClose,
		gifAutoPlay,
		isFocused = false,
		itemKey,
	}: {
		meme: FavoriteMemeRecord;
		coords: {
			position: 'absolute' | 'sticky';
			left?: number;
			right?: number;
			width: number;
			top?: number;
			height: number;
		};
		onClose?: () => void;
		gifAutoPlay: boolean;
		isFocused?: boolean;
		itemKey?: string;
	}) => {
		const {t, i18n} = useLingui();
		const videoContainerRef = React.useRef<HTMLDivElement>(null);
		const videoPool = useGifVideoPool();

		const isAudio = meme.contentType.startsWith('audio/');
		const isVideo = meme.contentType.startsWith('video/') || meme.contentType.includes('gif');

		const videoRef = usePooledVideo({
			src: !isAudio && isVideo ? meme.url : null,
			containerRef: videoContainerRef,
			videoPool,
			autoPlay: gifAutoPlay,
			enabled: !isAudio && isVideo,
		});

		const onMouseEnter = () => {
			if (gifAutoPlay) return;
			videoRef.current?.play().catch(() => {});
		};

		const onMouseLeave = () => {
			if (gifAutoPlay) return;
			const v = videoRef.current;
			if (!v) return;
			v.pause();
			try {
				v.currentTime = 0;
			} catch {}
		};

		const handleClick = (event: React.MouseEvent<HTMLDivElement> | React.KeyboardEvent<HTMLDivElement>) => {
			const shiftKey = 'shiftKey' in event ? event.shiftKey : false;

			MemesPickerStore.trackMemeUsage(meme.id);
			ComponentDispatch.dispatch('FAVORITE_MEME_SELECT', {meme, autoSend: !shiftKey});
			if (!shiftKey) onClose?.();
		};

		const handleEdit = (event: React.MouseEvent) => {
			event.stopPropagation();
			ModalActionCreators.push(modal(() => <EditFavoriteMemeModal meme={meme} />));
		};

		const handleDelete = (event: React.MouseEvent) => {
			event.stopPropagation();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Delete Saved Media`}
						description={t`Are you sure you want to delete "${meme.name}"? This action cannot be undone.`}
						primaryText={t`Delete`}
						primaryVariant="danger-primary"
						onPrimary={() => {
							FavoriteMemeActionCreators.deleteFavoriteMeme(i18n, meme.id);
						}}
					/>
				)),
			);
		};

		return (
			<div
				role="button"
				tabIndex={0}
				className={clsx(gifStyles.gridItem, gifStyles.gridItemGif, isFocused && gifStyles.gridItemFocused)}
				onClick={handleClick}
				onKeyDown={(event) => event.key === 'Enter' && handleClick(event)}
				onMouseEnter={onMouseEnter}
				onMouseLeave={onMouseLeave}
				style={coords}
				data-grid-item={itemKey}
			>
				{meme.isGifv && <GifIndicator />}

				<div className={gifStyles.gifMediaContainer}>
					{!isAudio && isVideo && <div ref={videoContainerRef} className={gifStyles.gifVideoContainer} />}

					{!isAudio && !isVideo && <img className={gifStyles.gif} src={meme.url} alt={meme.name} loading="lazy" />}

					{isAudio && (
						<div className={styles.audioCard}>
							<MusicNoteIcon className={styles.audioIcon} />
							<div className={styles.audioMeta}>
								{meme.duration && <div className={styles.audioDuration}>{formatDuration(meme.duration)}</div>}
								<Tooltip text={meme.filename}>
									<div className={styles.audioFilename}>{meme.filename}</div>
								</Tooltip>
								<div className={styles.audioBadge}>{getFileExtension(meme.filename, meme.contentType)}</div>
							</div>
						</div>
					)}
				</div>

				<div className={gifStyles.gridItemBackdrop} />

				<div className={gifStyles.hoverActionButtons}>
					<Tooltip text={t`Edit media`} position="bottom">
						<FocusRing offset={-2}>
							<button
								type="button"
								onClick={handleEdit}
								className={gifStyles.favoriteButton}
								aria-label={t`Edit saved media`}
							>
								<PencilSimpleIcon className={gifStyles.favoriteButtonIcon} weight="fill" />
							</button>
						</FocusRing>
					</Tooltip>
					<Tooltip text={t`Delete media`} position="bottom">
						<FocusRing offset={-2}>
							<button
								type="button"
								onClick={handleDelete}
								className={gifStyles.favoriteButton}
								style={{backgroundColor: 'var(--status-danger)', color: 'white'}}
								aria-label={t`Delete saved media`}
							>
								<TrashIcon className={gifStyles.favoriteButtonIcon} weight="fill" />
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>
		);
	},
);
