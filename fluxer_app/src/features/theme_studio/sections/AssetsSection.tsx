// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {createThemeAssetReference, createThemeLocalFileReference} from '@app/features/theme/utils/ThemeCssUtils';
import {showThemeStudioErrorModal} from '@app/features/theme_studio/utils/ThemeStudioErrorModalUtils';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {copyText} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CopyIcon, FilePlusIcon, ImageIcon, TrashIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';
import {broadcastThemeStudioMessage} from '../state/ThemeStudioBroadcast';
import {StudioButton} from '../ui/StudioButton';
import {StudioEmptyState} from '../ui/StudioEmptyState';
import {StudioIconButton} from '../ui/StudioIconButton';
import {StudioListItem} from '../ui/StudioListItem';
import {StudioPanel} from '../ui/StudioPanel';
import styles from './AssetsSection.module.css';

const UPLOADED_OTHER_DESCRIPTOR = msg({
	message: 'Uploaded {length, plural, one {# theme asset} other {# theme assets}}.',
	comment: 'Toast after uploading one or more Theme Studio asset files.',
});
const ADDED_OTHER_DESCRIPTOR = msg({
	message: 'Added {length, plural, one {# local file reference} other {# local file references}}.',
	comment: 'Toast after adding one or more desktop local file references for theme CSS.',
});
const REFERENCE_COPIED_DESCRIPTOR = msg({
	message: 'Reference copied.',
	comment: 'Short label in the theme studio assets section. Keep it concise.',
});
const WE_COULDN_T_COPY_THAT_REFERENCE_DESCRIPTOR = msg({
	message: "We couldn't copy that reference.",
	comment: 'Error message in the theme studio assets section.',
});
const UPLOADED_ASSETS_DESCRIPTOR = msg({
	message: 'Uploaded assets',
	comment: 'Button or menu action label in the theme studio assets section. Keep it concise.',
});
const NO_UPLOADED_ASSETS_DESCRIPTOR = msg({
	message: 'No assets yet',
	comment: 'Empty-state text in the theme studio assets section.',
});
const DROP_IMAGES_FONTS_OR_OTHER_FILES_HERE_TO_DESCRIPTOR = msg({
	message: 'Drop images, fonts, or other files here to use them inside your custom CSS.',
	comment: 'Description text in the theme studio assets section.',
});
const COPY_REFERENCE_FOR_DESCRIPTOR = msg({
	message: 'Copy reference for {fileName}',
	comment: 'Accessible label for copying a Theme Studio uploaded asset reference.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete {fileName}',
	comment: 'Accessible label for deleting a Theme Studio uploaded asset.',
});
const LOCAL_FILE_REFERENCES_DESCRIPTOR = msg({
	message: 'Local file references',
	comment: 'Short label in the theme studio assets section. Keep it concise.',
});
const NO_LOCAL_FILES_REFERENCED_DESCRIPTOR = msg({
	message: 'No local files referenced',
	comment: 'Empty-state text in the theme studio assets section.',
});
const ADD_FILES_FROM_YOUR_COMPUTER_TO_REFERENCE_THEM_DESCRIPTOR = msg({
	message: 'Add files from your computer to reference them in custom CSS without uploading.',
	comment: 'Button or menu action label in the theme studio assets section. Keep it concise.',
});
const AVAILABLE_IN_THE_DESKTOP_APP_DESCRIPTOR = msg({
	message: 'Available in the desktop app.',
	comment: 'Description text in the theme studio assets section.',
});
const COPY_REFERENCE_FOR_2_DESCRIPTOR = msg({
	message: 'Copy reference for {fileName}',
	comment: 'Accessible label for copying a Theme Studio local file reference.',
});
const DELETE_2_DESCRIPTOR = msg({
	message: 'Delete {fileName}',
	comment: 'Accessible label for deleting a Theme Studio local file reference.',
});
export const AssetsSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const assetInputRef = useRef<HTMLInputElement | null>(null);
	const handleAssetUpload = useCallback(
		(event: React.ChangeEvent<HTMLInputElement>) => {
			const files = [...(event.target.files ?? [])];
			event.target.value = '';
			if (files.length === 0) return;
			void ThemeLibrary.uploadAssets(files).then((assets) => {
				broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
				ToastCommands.success(i18n._(UPLOADED_OTHER_DESCRIPTOR, {length: assets.length}));
			});
		},
		[i18n],
	);
	const handleAddLocalFiles = useCallback(() => {
		void ThemeLibrary.addDesktopLocalFiles().then((files) => {
			if (files.length > 0) {
				broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
				ToastCommands.success(i18n._(ADDED_OTHER_DESCRIPTOR, {length: files.length}));
			}
		});
	}, [i18n]);
	const handleCopyReference = useCallback(
		(reference: string) => {
			void copyText(reference)
				.then(() => ToastCommands.success(i18n._(REFERENCE_COPIED_DESCRIPTOR)))
				.catch(() =>
					showThemeStudioErrorModal(
						i18n,
						() => i18n._(WE_COULDN_T_COPY_THAT_REFERENCE_DESCRIPTOR),
						'theme-studio.assets-section.copy-reference-error-modal',
					),
				);
		},
		[i18n],
	);
	const hasDesktopFileAccess = isDesktop() && Boolean(getElectronAPI()?.pickThemeLocalFiles);
	return (
		<div className={styles.section} data-flx="theme-studio.assets-section.section">
			<div className={styles.toolbar} data-flx="theme-studio.assets-section.toolbar">
				<StudioButton
					variant="secondary"
					compact
					leadingIcon={
						<UploadSimpleIcon
							size={remFromPx(13)}
							weight="bold"
							data-flx="theme-studio.assets-section.upload-simple-icon"
						/>
					}
					onClick={() => assetInputRef.current?.click()}
					data-flx="theme-studio.assets-section.studio-button.click"
				>
					<Trans>Upload assets</Trans>
				</StudioButton>
				{hasDesktopFileAccess ? (
					<StudioButton
						variant="secondary"
						compact
						leadingIcon={
							<FilePlusIcon size={remFromPx(13)} weight="bold" data-flx="theme-studio.assets-section.file-plus-icon" />
						}
						onClick={handleAddLocalFiles}
						data-flx="theme-studio.assets-section.studio-button.add-local-files"
					>
						<Trans>Add local files</Trans>
					</StudioButton>
				) : null}
				<input
					ref={assetInputRef}
					type="file"
					multiple
					hidden
					onChange={handleAssetUpload}
					aria-hidden
					data-flx="theme-studio.assets-section.input.asset-upload.file"
				/>
			</div>
			<div className={styles.body} data-flx="theme-studio.assets-section.body">
				<StudioPanel title={i18n._(UPLOADED_ASSETS_DESCRIPTOR)} data-flx="theme-studio.assets-section.studio-panel">
					{ThemeLibrary.assets.length === 0 ? (
						<div className={styles.empty} data-flx="theme-studio.assets-section.empty">
							<StudioEmptyState
								icon={
									<ImageIcon size={remFromPx(20)} weight="duotone" data-flx="theme-studio.assets-section.image-icon" />
								}
								title={i18n._(NO_UPLOADED_ASSETS_DESCRIPTOR)}
								description={i18n._(DROP_IMAGES_FONTS_OR_OTHER_FILES_HERE_TO_DESCRIPTOR)}
								data-flx="theme-studio.assets-section.studio-empty-state"
							/>
						</div>
					) : (
						<div className={styles.list} data-flx="theme-studio.assets-section.list">
							{ThemeLibrary.assets.map((asset) => {
								const reference = createThemeAssetReference(asset.name);
								return (
									<StudioListItem
										key={asset.id}
										leading={
											<ImageIcon
												size={remFromPx(16)}
												weight="duotone"
												data-flx="theme-studio.assets-section.image-icon--2"
											/>
										}
										label={asset.name}
										codeBody={reference}
										trailing={
											<>
												<StudioIconButton
													compact
													aria-label={i18n._(COPY_REFERENCE_FOR_DESCRIPTOR, {fileName: asset.name})}
													onClick={() => handleCopyReference(reference)}
													data-flx="theme-studio.assets-section.studio-icon-button.copy-reference"
												>
													<CopyIcon
														size={remFromPx(14)}
														weight="bold"
														data-flx="theme-studio.assets-section.copy-icon"
													/>
												</StudioIconButton>
												<StudioIconButton
													compact
													tone="danger"
													aria-label={i18n._(DELETE_DESCRIPTOR, {fileName: asset.name})}
													onClick={() => {
														void ThemeLibrary.deleteAsset(asset.id).then(() => {
															broadcastThemeStudioMessage({
																type: 'themeLibrary',
																revision: ThemeLibrary.revision,
															});
														});
													}}
													data-flx="theme-studio.assets-section.studio-icon-button"
												>
													<TrashIcon
														size={remFromPx(14)}
														weight="bold"
														data-flx="theme-studio.assets-section.trash-icon"
													/>
												</StudioIconButton>
											</>
										}
										data-flx="theme-studio.assets-section.studio-list-item"
									/>
								);
							})}
						</div>
					)}
				</StudioPanel>
				<StudioPanel
					title={i18n._(LOCAL_FILE_REFERENCES_DESCRIPTOR)}
					data-flx="theme-studio.assets-section.studio-panel--2"
				>
					{ThemeLibrary.localFiles.length === 0 ? (
						<div className={styles.empty} data-flx="theme-studio.assets-section.empty--2">
							<StudioEmptyState
								icon={
									<FilePlusIcon
										size={remFromPx(20)}
										weight="duotone"
										data-flx="theme-studio.assets-section.file-plus-icon--2"
									/>
								}
								title={i18n._(NO_LOCAL_FILES_REFERENCED_DESCRIPTOR)}
								description={
									hasDesktopFileAccess
										? i18n._(ADD_FILES_FROM_YOUR_COMPUTER_TO_REFERENCE_THEM_DESCRIPTOR)
										: i18n._(AVAILABLE_IN_THE_DESKTOP_APP_DESCRIPTOR)
								}
								data-flx="theme-studio.assets-section.studio-empty-state--2"
							/>
						</div>
					) : (
						<div className={styles.list} data-flx="theme-studio.assets-section.list--2">
							{ThemeLibrary.localFiles.map((file) => {
								const reference = createThemeLocalFileReference(file.path);
								return (
									<StudioListItem
										key={file.id}
										leading={
											<FilePlusIcon
												size={remFromPx(16)}
												weight="duotone"
												data-flx="theme-studio.assets-section.file-plus-icon--3"
											/>
										}
										label={file.name}
										codeBody={reference}
										trailing={
											<>
												<StudioIconButton
													compact
													aria-label={i18n._(COPY_REFERENCE_FOR_2_DESCRIPTOR, {fileName: file.name})}
													onClick={() => handleCopyReference(reference)}
													data-flx="theme-studio.assets-section.studio-icon-button.copy-reference--2"
												>
													<CopyIcon
														size={remFromPx(14)}
														weight="bold"
														data-flx="theme-studio.assets-section.copy-icon--2"
													/>
												</StudioIconButton>
												<StudioIconButton
													compact
													tone="danger"
													aria-label={i18n._(DELETE_2_DESCRIPTOR, {fileName: file.name})}
													onClick={() => {
														void ThemeLibrary.deleteLocalFile(file.id).then(() => {
															broadcastThemeStudioMessage({
																type: 'themeLibrary',
																revision: ThemeLibrary.revision,
															});
														});
													}}
													data-flx="theme-studio.assets-section.studio-icon-button--2"
												>
													<TrashIcon
														size={remFromPx(14)}
														weight="bold"
														data-flx="theme-studio.assets-section.trash-icon--2"
													/>
												</StudioIconButton>
											</>
										}
										data-flx="theme-studio.assets-section.studio-list-item--2"
									/>
								);
							})}
						</div>
					)}
				</StudioPanel>
			</div>
		</div>
	);
});
