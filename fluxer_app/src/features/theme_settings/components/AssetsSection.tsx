// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {createThemeAssetReference, createThemeLocalFileReference} from '@app/features/theme/utils/ThemeCssUtils';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {showThemeSettingsErrorModal} from '@app/features/theme_settings/utils/ThemeSettingsErrorModal';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {copyText} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {ASSETS_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CopyIcon, FilePlusIcon, TrashIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

const ASSETS_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Upload images and fonts to use in your CSS. Copy a file's reference and paste it into quick CSS or a theme.",
	comment: 'Description of the "Assets" section in Settings > Themes. Quick CSS is custom CSS the user writes.',
});
const UPLOADED_ASSETS_DESCRIPTOR = msg({
	message: 'Uploaded assets',
	comment: 'Heading in the "Assets" section in Settings > Themes.',
});
const LOCAL_FILE_REFERENCES_DESCRIPTOR = msg({
	message: 'Local file references',
	comment: 'Heading in the "Assets" section in Settings > Themes for files on this computer that CSS can use.',
});
const NO_UPLOADED_ASSETS_DESCRIPTOR = msg({
	message: 'No assets yet',
	comment: 'Empty state in the "Assets" section in Settings > Themes.',
});
const NO_LOCAL_FILES_REFERENCED_DESCRIPTOR = msg({
	message: 'No local files referenced',
	comment: 'Empty state for local file references in Settings > Themes.',
});
const AVAILABLE_IN_THE_DESKTOP_APP_DESCRIPTOR = msg({
	message: 'Available in the desktop app.',
	comment: 'Shown for local file references in Settings > Themes on the web app.',
});
const UPLOADED_ASSETS_TOAST_DESCRIPTOR = msg({
	message: 'Uploaded {length, plural, one {# theme asset} other {# theme assets}}.',
	comment: 'Toast after uploading one or more theme asset files.',
});
const ADDED_LOCAL_FILES_DESCRIPTOR = msg({
	message: 'Added {length, plural, one {# local file reference} other {# local file references}}.',
	comment: 'Toast after adding one or more desktop local file references for theme CSS.',
});
const REFERENCE_COPIED_DESCRIPTOR = msg({
	message: 'Reference copied.',
	comment: 'Toast after copying the CSS reference of a theme asset.',
});
const WE_COULDN_T_COPY_THAT_REFERENCE_DESCRIPTOR = msg({
	message: "We couldn't copy that reference.",
	comment: 'Error message after copying the CSS reference of a theme asset failed.',
});
const COPY_REFERENCE_FOR_DESCRIPTOR = msg({
	message: 'Copy reference for {fileName}',
	comment: 'Accessible label for copying the CSS reference of a theme asset or local file.',
});
const DELETE_FILE_DESCRIPTOR = msg({
	message: 'Delete {fileName}',
	comment: 'Accessible label for deleting a theme asset or local file reference.',
});

interface AssetRowProps {
	name: string;
	reference: string;
	onDelete: () => void;
}

const AssetRow: React.FC<AssetRowProps> = ({name, reference, onDelete}) => {
	const {i18n} = useLingui();
	const handleCopy = () => {
		void copyText(reference)
			.then(() => ToastCommands.success(i18n._(REFERENCE_COPIED_DESCRIPTOR)))
			.catch(() =>
				showThemeSettingsErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_COPY_THAT_REFERENCE_DESCRIPTOR),
					'theme-settings.assets-section.copy-reference-error-modal',
				),
			);
	};
	return (
		<div className={styles.row} data-flx="theme-settings.assets-section.asset-row.row">
			<div className={styles.rowText} data-flx="theme-settings.assets-section.asset-row.row-text">
				<span className={styles.rowLabel} data-flx="theme-settings.assets-section.asset-row.row-label">
					{name}
				</span>
				<span className={styles.rowMeta} data-flx="theme-settings.assets-section.asset-row.row-meta">
					{reference}
				</span>
			</div>
			<div className={styles.rowControls} data-flx="theme-settings.assets-section.asset-row.row-controls">
				<FocusRing offset={-2} data-flx="theme-settings.assets-section.asset-row.focus-ring">
					<button
						type="button"
						className={styles.iconButton}
						aria-label={i18n._(COPY_REFERENCE_FOR_DESCRIPTOR, {fileName: name})}
						onClick={handleCopy}
						data-flx="theme-settings.assets-section.asset-row.icon-button.copy"
					>
						<CopyIcon size={remFromPx(18)} data-flx="theme-settings.assets-section.asset-row.copy-icon" />
					</button>
				</FocusRing>
				<FocusRing offset={-2} data-flx="theme-settings.assets-section.asset-row.focus-ring--2">
					<button
						type="button"
						className={clsx(styles.iconButton, styles.iconButtonDanger)}
						aria-label={i18n._(DELETE_FILE_DESCRIPTOR, {fileName: name})}
						onClick={onDelete}
						data-flx="theme-settings.assets-section.asset-row.icon-button.delete"
					>
						<TrashIcon size={remFromPx(18)} data-flx="theme-settings.assets-section.asset-row.trash-icon" />
					</button>
				</FocusRing>
			</div>
		</div>
	);
};

export const AssetsSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const assetInputRef = useRef<HTMLInputElement | null>(null);
	const hasDesktopFileAccess = isDesktop() && Boolean(getElectronAPI()?.pickThemeLocalFiles);
	const handleUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
		const files = [...(event.target.files ?? [])];
		event.target.value = '';
		if (files.length === 0) return;
		void ThemeLibrary.uploadAssets(files).then((assets) => {
			ToastCommands.success(i18n._(UPLOADED_ASSETS_TOAST_DESCRIPTOR, {length: assets.length}));
		});
	};
	const handleAddLocalFiles = () => {
		void ThemeLibrary.addDesktopLocalFiles().then((files) => {
			if (files.length === 0) return;
			ToastCommands.success(i18n._(ADDED_LOCAL_FILES_DESCRIPTOR, {length: files.length}));
		});
	};
	const title = i18n._(ASSETS_SECTION_DESCRIPTOR);
	const description = i18n._(ASSETS_DESCRIPTION_DESCRIPTOR);
	const uploadedAssets = (
		<div className={styles.stack} data-flx="theme-settings.assets-section.stack">
			<div className={styles.subsectionHeader} data-flx="theme-settings.assets-section.subsection-header">
				<h4 className={styles.subsectionTitle} data-flx="theme-settings.assets-section.subsection-title">
					{i18n._(UPLOADED_ASSETS_DESCRIPTOR)}
				</h4>
				<div className={styles.subsectionActions} data-flx="theme-settings.assets-section.subsection-actions">
					<Button
						variant="secondary"
						small
						leftIcon={
							<UploadSimpleIcon size={remFromPx(16)} data-flx="theme-settings.assets-section.upload-simple-icon" />
						}
						onClick={() => assetInputRef.current?.click()}
						data-flx="theme-settings.assets-section.button.click"
					>
						<Trans>Upload assets</Trans>
					</Button>
					<input
						ref={assetInputRef}
						type="file"
						multiple
						hidden
						onChange={handleUpload}
						aria-hidden
						data-flx="theme-settings.assets-section.input.upload.file"
					/>
				</div>
			</div>
			{ThemeLibrary.assets.length === 0 ? (
				<p className={styles.note} data-flx="theme-settings.assets-section.note">
					{i18n._(NO_UPLOADED_ASSETS_DESCRIPTOR)}
				</p>
			) : (
				<div className={styles.list} data-flx="theme-settings.assets-section.list">
					{ThemeLibrary.assets.map((asset) => (
						<AssetRow
							key={asset.id}
							name={asset.name}
							reference={createThemeAssetReference(asset.name)}
							onDelete={() => {
								void ThemeLibrary.deleteAsset(asset.id);
							}}
							data-flx="theme-settings.assets-section.asset-row"
						/>
					))}
				</div>
			)}
		</div>
	);
	const localFileStackClass = styles.stack;
	const localFileHeader = (
		<div className={styles.subsectionHeader} data-flx="theme-settings.assets-section.subsection-header--2">
			<h4 className={styles.subsectionTitle} data-flx="theme-settings.assets-section.subsection-title--2">
				{i18n._(LOCAL_FILE_REFERENCES_DESCRIPTOR)}
			</h4>
			{hasDesktopFileAccess ? (
				<div className={styles.subsectionActions} data-flx="theme-settings.assets-section.subsection-actions--2">
					<Button
						variant="secondary"
						small
						leftIcon={<FilePlusIcon size={remFromPx(16)} data-flx="theme-settings.assets-section.file-plus-icon" />}
						onClick={handleAddLocalFiles}
						data-flx="theme-settings.assets-section.button.add-local-files"
					>
						<Trans>Add local files</Trans>
					</Button>
				</div>
			) : null}
		</div>
	);
	let localFileContent: React.ReactNode;
	if (!hasDesktopFileAccess) {
		localFileContent = (
			<p className={styles.note} data-flx="theme-settings.assets-section.note--2">
				{i18n._(AVAILABLE_IN_THE_DESKTOP_APP_DESCRIPTOR)}
			</p>
		);
	} else if (ThemeLibrary.localFiles.length === 0) {
		localFileContent = (
			<p className={styles.note} data-flx="theme-settings.assets-section.note--3">
				{i18n._(NO_LOCAL_FILES_REFERENCED_DESCRIPTOR)}
			</p>
		);
	} else {
		localFileContent = (
			<div className={styles.list} data-flx="theme-settings.assets-section.list--2">
				{ThemeLibrary.localFiles.map((file) => (
					<AssetRow
						key={file.id}
						name={file.name}
						reference={createThemeLocalFileReference(file.path)}
						onDelete={() => {
							void ThemeLibrary.deleteLocalFile(file.id);
						}}
						data-flx="theme-settings.assets-section.asset-row--2"
					/>
				))}
			</div>
		);
	}
	return (
		<SettingsSection
			id="assets"
			title={title}
			description={description}
			data-flx="theme-settings.assets-section.assets"
		>
			{uploadedAssets}
			<div className={localFileStackClass} data-flx="theme-settings.assets-section.stack--2">
				{localFileHeader}
				{localFileContent}
			</div>
		</SettingsSection>
	);
});
