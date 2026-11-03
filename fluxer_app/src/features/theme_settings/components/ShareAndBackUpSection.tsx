// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {downloadTextFile} from '@app/features/platform/utils/DownloadFile';
import {ShareThemeModal} from '@app/features/theme/components/modals/ShareThemeModal';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {useCurrentAccountLabels} from '@app/features/theme_settings/hooks/useCurrentAccountLabels';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {showThemeSettingsErrorModal} from '@app/features/theme_settings/utils/ThemeSettingsErrorModal';
import {
	EXPORT_LIBRARY_DESCRIPTOR,
	IMPORT_LIBRARY_DESCRIPTOR,
} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {readFileText} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {SHARE_AND_BACK_UP_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

const SHARE_YOUR_THEME_DESCRIPTOR = msg({
	message: 'Share your theme',
	comment: 'Row title in the "Share and back up" section in Settings > Themes.',
});
const SHARE_YOUR_THEME_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create a link to your theme or copy its CSS.',
	comment: 'Row description in the "Share and back up" section in Settings > Themes.',
});
const EXPORT_LIBRARY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Save all your themes and assets to one file.',
	comment: 'Row description for "Export library" in Settings > Themes.',
});
const IMPORT_LIBRARY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Add themes and assets from a library file.',
	comment: 'Row description for "Import library" in Settings > Themes.',
});
const RESET_THEME_DATA_DESCRIPTOR = msg({
	message: 'Reset theme data',
	comment: 'Row title in the "Share and back up" section in Settings > Themes.',
});
const RESET_DESKTOP_THEME_DATA_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Deletes the quick CSS of the desktop app theme, and every saved theme, asset and local file reference on this device.',
	comment:
		'Row description and confirmation text for "Reset theme data" in Settings > Themes while the desktop app theme is being edited. Quick CSS is custom CSS the user writes.',
});
const RESET_ACCOUNT_THEME_DATA_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Deletes the quick CSS for {username} on {instanceName}, and every saved theme, asset and local file reference on this device.',
	comment:
		'Row description and confirmation text for "Reset theme data" in Settings > Themes while an account\'s own theme is being edited. {username} is the account\'s username and {instanceName} is its instance. Quick CSS is custom CSS the user writes.',
});
const RESET_CUSTOM_THEME_DATA_DESCRIPTOR = msg({
	message: 'Reset custom theme data?',
	comment: 'Confirmation title before resetting theme data in Settings > Themes.',
});
const RESET_DATA_DESCRIPTOR = msg({
	message: 'Reset data',
	comment: 'Button that resets theme data in Settings > Themes.',
});
const NO_THEME_TO_SHARE_DESCRIPTOR = msg({
	message: 'Turn on a saved theme or write some quick CSS before sharing.',
	comment: 'Error shown when sharing a theme in Settings > Themes while nothing is turned on and quick CSS is empty.',
});
const IMPORTED_LIBRARY_DESCRIPTOR = msg({
	message:
		'Imported {themes, plural, one {# theme} other {# themes}} and {assets, plural, one {# asset} other {# assets}}.',
	comment: 'Toast after importing a theme library file.',
});
const WE_COULDN_T_IMPORT_THAT_THEME_LIBRARY_FILE_DESCRIPTOR = msg({
	message: "We couldn't import that theme library file.",
	comment: 'Error message after importing a theme library file failed.',
});

interface ActionRowProps {
	title: string;
	description: string;
	children: React.ReactNode;
}

const ActionRow: React.FC<ActionRowProps> = ({title, description, children}) => (
	<div className={styles.row} data-flx="theme-settings.share-and-back-up-section.action-row.row">
		<div className={styles.rowText} data-flx="theme-settings.share-and-back-up-section.action-row.row-text">
			<span className={styles.rowLabel} data-flx="theme-settings.share-and-back-up-section.action-row.row-label">
				{title}
			</span>
			<span
				className={styles.rowDescription}
				data-flx="theme-settings.share-and-back-up-section.action-row.row-description"
			>
				{description}
			</span>
		</div>
		<div className={styles.rowControls} data-flx="theme-settings.share-and-back-up-section.action-row.row-controls">
			{children}
		</div>
	</div>
);

export const ShareAndBackUpSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {username, instanceName} = useCurrentAccountLabels();
	const resetDescription =
		ThemeEditTarget.target === 'desktop'
			? i18n._(RESET_DESKTOP_THEME_DATA_DESCRIPTION_DESCRIPTOR)
			: i18n._(RESET_ACCOUNT_THEME_DATA_DESCRIPTION_DESCRIPTOR, {username, instanceName});
	const libraryImportRef = useRef<HTMLInputElement | null>(null);
	const handleShare = () => {
		const target = ThemeEditTarget.target;
		const css = [ThemeLibrary.themeCssFor(ThemeLibrary.enabledIdsFor(target)), ThemeProfile.customThemeCssFor(target)]
			.map((value) => value?.trim() ?? '')
			.filter(Boolean)
			.join('\n\n');
		if (!css) {
			showThemeSettingsErrorModal(
				i18n,
				() => i18n._(NO_THEME_TO_SHARE_DESCRIPTOR),
				'theme-settings.share-and-back-up-section.share-empty-error-modal',
			);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ShareThemeModal
					themeCss={css}
					data-flx="theme-settings.share-and-back-up-section.handle-share.share-theme-modal"
				/>
			)),
		);
	};
	const handleExportLibrary = () => {
		void ThemeLibrary.buildExportPayload().then((payload) => {
			downloadTextFile(JSON.stringify(payload, null, 2), 'fluxer-theme-library.json', 'application/json;charset=utf-8');
		});
	};
	const handleImportLibrary = (event: React.ChangeEvent<HTMLInputElement>) => {
		const file = event.target.files?.[0];
		event.target.value = '';
		if (!file) return;
		void readFileText(file)
			.then((text) => ThemeLibrary.importExportPayload(JSON.parse(text)))
			.then((result) => {
				ToastCommands.success(i18n._(IMPORTED_LIBRARY_DESCRIPTOR, {themes: result.themes, assets: result.assets}));
			})
			.catch(() =>
				showThemeSettingsErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_IMPORT_THAT_THEME_LIBRARY_FILE_DESCRIPTOR),
					'theme-settings.share-and-back-up-section.import-library-error-modal',
				),
			);
	};
	const handleReset = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(RESET_CUSTOM_THEME_DATA_DESCRIPTOR)}
					description={resetDescription}
					primaryText={i18n._(RESET_DATA_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => {
						ThemeSettingsCommands.setEditedCustomThemeCss(null);
						void ThemeLibrary.resetLibrary();
					}}
					data-flx="theme-settings.share-and-back-up-section.handle-reset.confirm-modal"
				/>
			)),
		);
	};
	return (
		<SettingsSection
			id="share-and-back-up"
			title={i18n._(SHARE_AND_BACK_UP_SECTION_DESCRIPTOR)}
			data-flx="theme-settings.share-and-back-up-section.share-and-back-up"
		>
			<div className={styles.list} data-flx="theme-settings.share-and-back-up-section.list">
				<ActionRow
					title={i18n._(SHARE_YOUR_THEME_DESCRIPTOR)}
					description={i18n._(SHARE_YOUR_THEME_DESCRIPTION_DESCRIPTOR)}
					data-flx="theme-settings.share-and-back-up-section.action-row"
				>
					<Button
						variant="secondary"
						small
						onClick={handleShare}
						data-flx="theme-settings.share-and-back-up-section.button.share"
					>
						<Trans>Share theme</Trans>
					</Button>
				</ActionRow>
				<ActionRow
					title={i18n._(EXPORT_LIBRARY_DESCRIPTOR)}
					description={i18n._(EXPORT_LIBRARY_DESCRIPTION_DESCRIPTOR)}
					data-flx="theme-settings.share-and-back-up-section.action-row--2"
				>
					<Button
						variant="secondary"
						small
						onClick={handleExportLibrary}
						data-flx="theme-settings.share-and-back-up-section.button.export-library"
					>
						{i18n._(EXPORT_LIBRARY_DESCRIPTOR)}
					</Button>
				</ActionRow>
				<ActionRow
					title={i18n._(IMPORT_LIBRARY_DESCRIPTOR)}
					description={i18n._(IMPORT_LIBRARY_DESCRIPTION_DESCRIPTOR)}
					data-flx="theme-settings.share-and-back-up-section.action-row--3"
				>
					<Button
						variant="secondary"
						small
						onClick={() => libraryImportRef.current?.click()}
						data-flx="theme-settings.share-and-back-up-section.button.click"
					>
						{i18n._(IMPORT_LIBRARY_DESCRIPTOR)}
					</Button>
					<input
						ref={libraryImportRef}
						type="file"
						accept=".json,application/json,text/plain"
						hidden
						onChange={handleImportLibrary}
						aria-hidden
						data-flx="theme-settings.share-and-back-up-section.input.import-library.file"
					/>
				</ActionRow>
				<ActionRow
					title={i18n._(RESET_THEME_DATA_DESCRIPTOR)}
					description={resetDescription}
					data-flx="theme-settings.share-and-back-up-section.action-row--4"
				>
					<Button
						variant="danger"
						small
						onClick={handleReset}
						data-flx="theme-settings.share-and-back-up-section.button.reset"
					>
						{i18n._(RESET_DATA_DESCRIPTOR)}
					</Button>
				</ActionRow>
			</div>
		</SettingsSection>
	);
});
