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
import {MagnifyingGlassIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildStickerActionCreators from '~/actions/GuildStickerActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {STICKER_MAX_SIZE} from '~/Constants';
import {Input} from '~/components/form/Input';
import {UploadDropZone} from '~/components/guild/UploadDropZone';
import {UploadSlotInfo} from '~/components/guild/UploadSlotInfo';
import {AddGuildStickerModal} from '~/components/modals/AddGuildStickerModal';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {StickerGridItem} from '~/components/stickers/StickerGridItem';
import {Spinner} from '~/components/uikit/Spinner';
import type {GuildStickerWithUser} from '~/records/GuildStickerRecord';
import EmojiStickerLayoutStore from '~/stores/EmojiStickerLayoutStore';
import {seedGuildStickerCache, subscribeToGuildStickerUpdates} from '~/stores/GuildExpressionTabCache';
import GuildStore from '~/stores/GuildStore';
import {openFilePicker} from '~/utils/FilePickerUtils';
import {sortBySnowflakeDesc} from '~/utils/SnowflakeUtils';
import styles from './GuildStickersTab.module.css';

const GuildStickersTab: React.FC<{guildId: string}> = observer(function GuildStickersTab({guildId}) {
	const {t} = useLingui();
	const [stickers, setStickers] = React.useState<ReadonlyArray<GuildStickerWithUser>>([]);
	const [fetchStatus, setFetchStatus] = React.useState<'idle' | 'pending' | 'success' | 'error'>('idle');
	const [searchQuery, setSearchQuery] = React.useState('');
	const layoutStore = EmojiStickerLayoutStore;
	const viewMode = layoutStore.getStickerViewMode();
	const guild = GuildStore.getGuild(guildId);
	const setStickersWithCache = React.useCallback(
		(updater: React.SetStateAction<ReadonlyArray<GuildStickerWithUser>>) => {
			setStickers((prev) => {
				const next =
					typeof updater === 'function'
						? (updater as (previous: ReadonlyArray<GuildStickerWithUser>) => ReadonlyArray<GuildStickerWithUser>)(prev)
						: updater;
				const frozen = Object.freeze(sortBySnowflakeDesc(next));
				seedGuildStickerCache(guildId, frozen);
				return frozen;
			});
		},
		[guildId],
	);

	const fetchStickers = React.useCallback(async () => {
		try {
			setFetchStatus('pending');
			const stickerList = await GuildStickerActionCreators.list(guildId);
			setStickersWithCache(stickerList);
			setFetchStatus('success');
		} catch (error) {
			console.error('Failed to fetch stickers:', error);
			setFetchStatus('error');
		}
	}, [guildId, setStickersWithCache]);

	React.useEffect(() => {
		if (fetchStatus === 'idle') {
			void fetchStickers();
		}
	}, [fetchStatus, fetchStickers]);

	React.useEffect(() => {
		return subscribeToGuildStickerUpdates(guildId, (updatedStickers) => {
			setStickersWithCache(updatedStickers);
		});
	}, [guildId, setStickersWithCache]);

	const handleAddSticker = async () => {
		const [file] = await openFilePicker({
			accept: '.jpg,.jpeg,.png,.gif,.webp,image/*',
		});
		if (file) {
			ModalActionCreators.push(
				modal(() => <AddGuildStickerModal guildId={guildId} file={file} onSuccess={fetchStickers} />),
			);
		}
	};

	const handleDrop = (files: Array<File>) => {
		const file = files[0];
		if (file) {
			ModalActionCreators.push(
				modal(() => <AddGuildStickerModal guildId={guildId} file={file} onSuccess={fetchStickers} />),
			);
		}
	};

	const filteredStickers = React.useMemo(() => {
		if (!searchQuery) return stickers;
		return matchSorter(stickers, searchQuery, {
			keys: [(sticker) => sticker.name],
		});
	}, [stickers, searchQuery]);

	const maxStickers = guild?.maxStickers ?? 50;

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Custom Stickers</Trans>
				</h2>
				<p className={styles.subtitle}>
					<Trans>Upload and manage custom stickers for your community.</Trans>
				</p>
			</div>

			<div className={styles.controls}>
				<Input
					type="text"
					placeholder={t`Search stickers...`}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
					className={styles.searchInput}
				/>

				<div className={styles.viewToggle} role="group" aria-label={t`Sticker density`}>
					<button
						type="button"
						onClick={() => layoutStore.setStickerViewMode('cozy')}
						className={clsx(styles.viewToggleButton, viewMode === 'cozy' && styles.viewToggleButtonActive)}
						aria-pressed={viewMode === 'cozy'}
					>
						<Trans>Cozy</Trans>
					</button>
					<button
						type="button"
						onClick={() => layoutStore.setStickerViewMode('compact')}
						className={clsx(styles.viewToggleButton, viewMode === 'compact' && styles.viewToggleButtonActive)}
						aria-pressed={viewMode === 'compact'}
					>
						<Trans>Compact</Trans>
					</button>
				</div>
			</div>

			<UploadSlotInfo
				title={<Trans>Sticker Slots</Trans>}
				currentCount={stickers.length}
				maxCount={maxStickers}
				uploadButtonText={<Trans>Upload Sticker</Trans>}
				onUploadClick={handleAddSticker}
				description={
					<Trans>
						Stickers must be exactly 320x320 pixels and no larger than {STICKER_MAX_SIZE / 1024} KB, but we
						automatically resize and compress images for you. Allowed file types: JPEG, PNG, WebP, GIF.
					</Trans>
				}
			/>
			<UploadDropZone
				onDrop={handleDrop}
				description={<Trans>Drag and drop a sticker file here (one at a time)</Trans>}
				acceptMultiple={false}
			/>

			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer}>
					<Spinner />
				</div>
			)}

			{searchQuery && filteredStickers.length === 0 && (
				<StatusSlate
					Icon={MagnifyingGlassIcon}
					title={t`No stickers found`}
					description={t`No stickers found matching your search.`}
					fullHeight={true}
				/>
			)}

			{fetchStatus === 'success' && filteredStickers.length > 0 && (
				<div className={clsx(styles.stickerGrid, viewMode === 'compact' ? styles.compactGrid : styles.cozyGrid)}>
					{filteredStickers.map((sticker) => (
						<StickerGridItem key={sticker.id} guildId={guildId} sticker={sticker} onUpdate={fetchStickers} />
					))}
				</div>
			)}

			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningCircleIcon}
					title={t`Failed to load stickers`}
					description={t`There was an error loading the stickers. Please try again.`}
					actions={[
						{
							text: t`Retry`,
							onClick: fetchStickers,
							variant: 'primary',
						},
					]}
					fullHeight={true}
				/>
			)}
		</div>
	);
});

export default GuildStickersTab;
