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
import * as GuildEmojiActionCreators from '~/actions/GuildEmojiActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes, EMOJI_MAX_SIZE} from '~/Constants';
import {EmojiListHeader, EmojiListItem} from '~/components/emojis/EmojiListItem';
import {Input} from '~/components/form/Input';
import {UploadDropZone} from '~/components/guild/UploadDropZone';
import {UploadSlotInfo} from '~/components/guild/UploadSlotInfo';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {EmojiUploadModal} from '~/components/modals/EmojiUploadModal';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Spinner} from '~/components/uikit/Spinner';
import type {GuildEmojiWithUser} from '~/records/GuildEmojiRecord';
import EmojiStickerLayoutStore from '~/stores/EmojiStickerLayoutStore';
import {seedGuildEmojiCache, subscribeToGuildEmojiUpdates} from '~/stores/GuildExpressionTabCache';
import GuildStore from '~/stores/GuildStore';
import {openFilePicker} from '~/utils/FilePickerUtils';
import * as ImageCropUtils from '~/utils/ImageCropUtils';
import {sortBySnowflakeDesc} from '~/utils/SnowflakeUtils';
import styles from './GuildEmojiTab.module.css';

const MAX_EMOJIS_PER_UPLOAD = 50;

type ValidationError = {path: string; message: string};
type APIErrorBody = {code?: unknown; errors?: Array<ValidationError>};

const GuildEmojiTab: React.FC<{guildId: string}> = observer(function GuildEmojiTab({guildId}) {
	const {t} = useLingui();
	const [emojis, setEmojis] = React.useState<ReadonlyArray<GuildEmojiWithUser>>([]);
	const [fetchStatus, setFetchStatus] = React.useState<'idle' | 'pending' | 'success' | 'error'>('idle');
	const [searchQuery, setSearchQuery] = React.useState('');

	const layoutStore = EmojiStickerLayoutStore;
	const layout = layoutStore.getEmojiLayout();
	const guild = GuildStore.getGuild(guildId);

	const setEmojisWithCache = React.useCallback(
		(updater: React.SetStateAction<ReadonlyArray<GuildEmojiWithUser>>) => {
			setEmojis((prev) => {
				const next =
					typeof updater === 'function'
						? (updater as (previous: ReadonlyArray<GuildEmojiWithUser>) => ReadonlyArray<GuildEmojiWithUser>)(prev)
						: updater;

				const frozen = Object.freeze(sortBySnowflakeDesc(next));
				seedGuildEmojiCache(guildId, frozen);
				return frozen;
			});
		},
		[guildId],
	);

	const fetchEmojis = React.useCallback(async () => {
		try {
			setFetchStatus('pending');
			const emojiList = await GuildEmojiActionCreators.list(guildId);
			setEmojisWithCache(emojiList);
			setFetchStatus('success');
		} catch (error) {
			console.error('Failed to fetch emojis:', error);
			setFetchStatus('error');
		}
	}, [guildId, setEmojisWithCache]);

	React.useEffect(() => {
		if (fetchStatus === 'idle') {
			void fetchEmojis();
		}
	}, [fetchStatus, fetchEmojis]);

	React.useEffect(() => {
		return subscribeToGuildEmojiUpdates(guildId, (updatedEmojis) => {
			setEmojisWithCache(updatedEmojis);
		});
	}, [guildId, setEmojisWithCache]);

	const handleRename = React.useCallback(
		(emojiId: string, newName: string) => {
			setEmojisWithCache((prev) => prev.map((e) => (e.id === emojiId ? {...e, name: newName} : e)));
		},
		[setEmojisWithCache],
	);

	const handleRemove = React.useCallback(
		(emojiId: string) => {
			setEmojisWithCache((prev) => prev.filter((e) => e.id !== emojiId));
		},
		[setEmojisWithCache],
	);

	const processAndUploadEmojis = async (files: Array<File>) => {
		const filesToProcess = files.slice(0, MAX_EMOJIS_PER_UPLOAD);

		if (files.length > MAX_EMOJIS_PER_UPLOAD) {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Too Many Files`}
						description={t`You can only upload up to ${MAX_EMOJIS_PER_UPLOAD} emojis at once. Only the first ${MAX_EMOJIS_PER_UPLOAD} will be processed.`}
						primaryText={t`OK`}
						primaryVariant="primary"
						onPrimary={() => {}}
					/>
				)),
			);
		}

		const validFiles = filesToProcess.filter((file) => {
			const ext = file.name.split('.').pop()?.toLowerCase();
			return ext != null && ['jpeg', 'jpg', 'png', 'gif', 'webp'].includes(ext);
		});

		if (validFiles.length === 0) {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Invalid File Type`}
						description={t`Please upload JPEG, PNG, WebP, or GIF files only.`}
						primaryText={t`OK`}
						primaryVariant="primary"
						onPrimary={() => {}}
					/>
				)),
			);
			return;
		}

		ModalActionCreators.push(modal(() => <EmojiUploadModal count={validFiles.length} />));

		const preparedEmojis: Array<{name: string; image: string; file: File}> = [];
		const preparationFailures: Array<{name: string; error: string}> = [];

		for (const file of validFiles) {
			try {
				const base64Image = await ImageCropUtils.optimizeEmojiImage(file, EMOJI_MAX_SIZE, 128);
				const name = GuildEmojiActionCreators.sanitizeEmojiName(file.name);
				preparedEmojis.push({name, image: base64Image, file});
			} catch (error) {
				console.error(`Failed to prepare emoji ${file.name}:`, error);
				preparationFailures.push({
					name: file.name,
					error: error instanceof Error ? error.message : String(error),
				});
			}
		}

		if (preparedEmojis.length === 0) {
			ModalActionCreators.pop();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Failed to Prepare Emojis`}
						description={
							<div className={styles.modalErrorContainer}>
								<p className={styles.modalErrorIntro}>
									<Trans>Unable to prepare any emojis for upload. The following errors occurred:</Trans>
								</p>
								{preparationFailures.map((failed, index) => (
									<div key={index} className={styles.modalErrorItem}>
										<div className={styles.modalErrorDetails}>
											<div className={styles.modalErrorName}>{failed.name}</div>
											<div className={styles.modalErrorMessage}>{failed.error}</div>
										</div>
									</div>
								))}
							</div>
						}
						primaryText={t`OK`}
						primaryVariant="primary"
						onPrimary={() => {}}
					/>
				)),
			);
			return;
		}

		try {
			const result = await GuildEmojiActionCreators.bulkUpload(
				guildId,
				preparedEmojis.map(({name, image}) => ({name, image})),
			);

			await fetchEmojis();

			ModalActionCreators.pop();

			const successCount = result.success.length;
			const totalAttempted = validFiles.length;
			const totalFailures = result.failed.length + preparationFailures.length;

			if (totalFailures === 0) {
				ToastActionCreators.success(
					successCount === 1
						? t`Successfully uploaded ${successCount} emoji.`
						: t`Successfully uploaded ${successCount} emojis.`,
				);
			} else {
				ToastActionCreators.success(
					totalAttempted === 1
						? t`Successfully uploaded ${successCount} of ${totalAttempted} emoji.`
						: t`Successfully uploaded ${successCount} of ${totalAttempted} emojis.`,
				);
			}

			if (result.failed.length > 0 || preparationFailures.length > 0) {
				const failedWithImages = result.failed.map((failed) => {
					const emoji = preparedEmojis.find((e) => e.name === failed.name);
					return {
						...failed,
						imageUrl: emoji?.image ?? null,
						mimeType: emoji?.file.type ?? 'image/png',
					};
				});

				const allFailures = [
					...preparationFailures.map((failed) => ({
						name: failed.name,
						error: failed.error,
						imageUrl: null as string | null,
						mimeType: 'image/png',
					})),
					...failedWithImages,
				];

				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`Some Emojis Failed to Upload`}
							description={
								<div className={styles.modalErrorContainer}>
									<p className={styles.modalErrorIntro}>
										<Trans>
											{successCount} of {totalAttempted} emojis uploaded successfully. The following emojis failed:
										</Trans>
									</p>
									{allFailures.map((failed, index) => (
										<div key={index} className={styles.modalErrorItem}>
											{failed.imageUrl && (
												<img
													src={`data:${failed.mimeType};base64,${failed.imageUrl}`}
													alt={failed.name}
													className={styles.modalErrorImage}
												/>
											)}
											<div className={styles.modalErrorDetails}>
												<div className={styles.modalErrorName}>{failed.name}</div>
												<div className={styles.modalErrorMessage}>{failed.error}</div>
											</div>
										</div>
									))}
								</div>
							}
							primaryText={t`OK`}
							primaryVariant="primary"
							onPrimary={() => {}}
						/>
					)),
				);
			}
		} catch (error: unknown) {
			console.error('Failed to upload emojis:', error);
			ModalActionCreators.pop();

			const errorData: APIErrorBody | null =
				typeof error === 'object' && error !== null && 'body' in error ? ((error as any).body as APIErrorBody) : null;

			if (
				errorData?.code === APIErrorCodes.INVALID_FORM_BODY &&
				Array.isArray(errorData.errors) &&
				preparedEmojis.length > 0
			) {
				const failedEmojiIndexes = new Set<number>();
				const validationErrors = new Map<number, string>();

				for (const validationError of errorData.errors) {
					const pathMatch = validationError.path.match(/^emojis\.(\d+)\./);
					if (pathMatch) {
						const index = Number.parseInt(pathMatch[1], 10);
						if (!Number.isNaN(index) && index >= 0 && index < preparedEmojis.length) {
							failedEmojiIndexes.add(index);
							validationErrors.set(index, validationError.message);
						}
					}
				}

				if (failedEmojiIndexes.size > 0) {
					const failedEmojis = Array.from(failedEmojiIndexes).map((index) => ({
						...preparedEmojis[index],
						error: validationErrors.get(index) ?? t`Unknown error`,
						mimeType: preparedEmojis[index].file.type,
					}));

					ModalActionCreators.push(
						modal(() => (
							<ConfirmModal
								title={t`Some Emojis Failed to Upload`}
								description={
									<div className={styles.modalErrorContainer}>
										<p className={styles.modalErrorIntro}>
											<Trans>
												{preparedEmojis.length - failedEmojiIndexes.size} of {preparedEmojis.length} emojis were not
												uploaded due to validation errors. The following emojis failed:
											</Trans>
										</p>
										{failedEmojis.map((failed, index) => (
											<div key={index} className={styles.modalErrorItem}>
												<img
													src={`data:${failed.mimeType};base64,${failed.image}`}
													alt={failed.name}
													className={styles.modalErrorImage}
												/>
												<div className={styles.modalErrorDetails}>
													<div className={styles.modalErrorName}>{failed.name}</div>
													<div className={styles.modalErrorMessage}>{failed.error}</div>
												</div>
											</div>
										))}
									</div>
								}
								primaryText={t`OK`}
								primaryVariant="primary"
								onPrimary={() => {}}
							/>
						)),
					);
					return;
				}
			}

			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Upload Failed`}
						description={t`Failed to upload emojis. Please try again.`}
						primaryText={t`OK`}
						primaryVariant="primary"
						onPrimary={() => {}}
					/>
				)),
			);
		}
	};

	const handleFileSelect = (files: Array<File>) => {
		if (files.length > 0) {
			void processAndUploadEmojis(files);
		}
	};

	const handleDrop = (files: Array<File>) => {
		void processAndUploadEmojis(files);
	};

	const filteredEmojis = React.useMemo(() => {
		if (!searchQuery) return emojis;
		return matchSorter(emojis, searchQuery, {
			keys: [(emoji) => emoji.name],
		});
	}, [emojis, searchQuery]);

	const animatedEmojis = React.useMemo(() => filteredEmojis.filter((emoji) => emoji.animated), [filteredEmojis]);
	const staticEmojis = React.useMemo(() => filteredEmojis.filter((emoji) => !emoji.animated), [filteredEmojis]);

	const maxStaticEmojis = guild?.maxStaticEmojis ?? 50;
	const maxAnimatedEmojis = guild?.maxAnimatedEmojis ?? 50;

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Custom Emoji</Trans>
				</h2>
				<p className={styles.subtitle}>
					<Trans>Upload and manage custom emojis for your community.</Trans>
				</p>
			</div>

			<div className={styles.controls}>
				<Input
					type="text"
					placeholder={t`Search emojis...`}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
					className={styles.searchInput}
				/>

				<div className={styles.layoutControls} role="group" aria-label={t`Emoji layout`}>
					<button
						type="button"
						onClick={() => layoutStore.setEmojiLayout('list')}
						className={clsx(styles.layoutToggleButton, layout === 'list' && styles.layoutToggleButtonActive)}
						aria-pressed={layout === 'list'}
					>
						<Trans>Row</Trans>
					</button>
					<button
						type="button"
						onClick={() => layoutStore.setEmojiLayout('grid')}
						className={clsx(styles.layoutToggleButton, layout === 'grid' && styles.layoutToggleButtonActive)}
						aria-pressed={layout === 'grid'}
					>
						<Trans>Grid</Trans>
					</button>
				</div>
			</div>

			<UploadSlotInfo
				title={<Trans>Emoji Slots</Trans>}
				currentCount={emojis.length}
				maxCount={maxStaticEmojis}
				uploadButtonText={<Trans>Upload Emoji</Trans>}
				onUploadClick={async () => {
					const files = await openFilePicker({
						multiple: true,
						accept: '.jpg,.jpeg,.png,.gif,.webp,image/*',
					});
					handleFileSelect(files);
				}}
				description={
					<Trans>
						Emoji names must be at least 2 characters long and can only contain alphanumeric characters and underscores.
						Allowed file types: JPEG, PNG, WebP, GIF. We compress images to 128x128 pixels. Maximum size:{' '}
						{Math.round(EMOJI_MAX_SIZE / 1024)} KB per emoji.
					</Trans>
				}
				additionalSlots={
					<>
						<span>
							<Trans>
								Static: {staticEmojis.length} / {maxStaticEmojis === Number.POSITIVE_INFINITY ? '∞' : maxStaticEmojis}
							</Trans>
						</span>
						<span>
							<Trans>
								Animated: {animatedEmojis.length} /{' '}
								{maxAnimatedEmojis === Number.POSITIVE_INFINITY ? '∞' : maxAnimatedEmojis}
							</Trans>
						</span>
					</>
				}
			/>

			<UploadDropZone onDrop={handleDrop} description={<Trans>Drag and drop emoji files here</Trans>} />

			{searchQuery && filteredEmojis.length === 0 && (
				<div className={styles.notice}>
					<p className={styles.noticeText}>
						<Trans>No emojis found matching your search.</Trans>
					</p>
				</div>
			)}

			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer}>
					<Spinner />
				</div>
			)}

			{fetchStatus === 'success' && (staticEmojis.length > 0 || animatedEmojis.length > 0) && (
				<div className={clsx(styles.emojiSections, layout === 'grid' && styles.emojiSectionsGrid)}>
					{staticEmojis.length > 0 && (
						<div className={styles.emojiSection}>
							<h3 className={styles.emojiSectionTitle}>
								<Trans>Non-Animated Emoji ({staticEmojis.length})</Trans>
							</h3>
							{layout === 'list' && <EmojiListHeader />}
							<div className={layout === 'list' ? styles.emojiItemsList : styles.emojiGrid}>
								{staticEmojis.map((emoji) => (
									<EmojiListItem
										key={emoji.id}
										guildId={guildId}
										emoji={emoji}
										layout={layout}
										onRename={handleRename}
										onRemove={handleRemove}
									/>
								))}
							</div>
						</div>
					)}

					{animatedEmojis.length > 0 && (
						<div className={styles.emojiSection}>
							<h3 className={styles.emojiSectionTitle}>
								<Trans>Animated Emoji ({animatedEmojis.length})</Trans>
							</h3>
							{layout === 'list' && <EmojiListHeader />}
							<div className={layout === 'list' ? styles.emojiItemsList : styles.emojiGrid}>
								{animatedEmojis.map((emoji) => (
									<EmojiListItem
										key={emoji.id}
										guildId={guildId}
										emoji={emoji}
										layout={layout}
										onRename={handleRename}
										onRemove={handleRemove}
									/>
								))}
							</div>
						</div>
					)}
				</div>
			)}

			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningCircleIcon}
					title={t`Failed to load emojis`}
					description={t`There was an error loading the emojis. Please try again.`}
					actions={[
						{
							text: t`Retry`,
							onClick: fetchEmojis,
							variant: 'primary',
						},
					]}
					fullHeight={true}
				/>
			)}
		</div>
	);
});

export default GuildEmojiTab;
