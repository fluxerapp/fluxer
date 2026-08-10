// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {DESKTOP_DOWNLOAD_URL, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {UpdaterDownloadOption} from '@app/features/platform/types/Electron';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {openExternalUrl, isCanaryDesktop} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/updater/commands/UpdaterModalCommands.module.css';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useMemo, useState} from 'react';

const DESKTOP_UPDATE_AVAILABLE_DESCRIPTOR = msg({
	message: 'Desktop update available',
	comment: 'Modal title shown when a new desktop app update can be downloaded.',
});
const DESKTOP_VERSION_IS_READY_TO_DOWNLOAD_DESCRIPTOR = msg({
	message: 'Desktop version {version} is available. Download the installer, then run it to update {productName}.',
	comment:
		'Desktop updater modal body. The version placeholder is the target app version; productName is the app name.',
});
const A_NEW_DESKTOP_VERSION_IS_READY_TO_DOWNLOAD_DESCRIPTOR = msg({
	message: 'A new desktop version is available. Download the installer, then run it to update {productName}.',
	comment: 'Desktop updater modal body when the exact target version is unknown. productName is the app name.',
});
const DOWNLOAD_INSTALLER_DESCRIPTOR = msg({
	message: 'Download installer',
	comment: 'Button label that downloads a desktop update installer.',
});
const DOWNLOAD_PACKAGE_DESCRIPTOR = msg({
	message: 'Download package',
	comment: 'Button label that downloads a selected Linux desktop update package.',
});
const LATER_DESCRIPTOR = msg({
	message: 'Later',
	comment: 'Button label that dismisses the update modal without taking action.',
});
const DESKTOP_APP_IS_UP_TO_DATE_DESCRIPTOR = msg({
	message: 'Desktop app is up to date',
	comment: 'Modal title shown after a manual desktop update check finds no update.',
});
const INSTALLED_VERSION_IS_CURRENT_DESCRIPTOR = msg({
	message: 'Installed version: {currentVersion}.',
	comment: 'Desktop updater modal body. The currentVersion placeholder is the installed app version.',
});
const NO_DESKTOP_UPDATE_IS_AVAILABLE_DESCRIPTOR = msg({
	message: 'No desktop update is available.',
	comment: 'Desktop updater modal body when no installed version string is available.',
});
const MANUAL_DESKTOP_UPDATE_DESCRIPTOR = msg({
	message: 'Manual desktop update',
	comment: 'Modal title when automatic in-app desktop updates cannot be used.',
});
const DEVELOPMENT_BUILD_DESCRIPTOR = msg({
	message: 'Development build',
	comment: 'Modal title when the desktop app is running unpackaged during development.',
});
const SYSTEM_MANAGED_INSTALL_DESCRIPTOR = msg({
	message: 'System-managed install',
	comment: 'Modal title when the desktop app is installed through a system package manager such as Flatpak.',
});
const DEVELOPMENT_BUILD_UPDATE_BODY_DESCRIPTOR = msg({
	message: 'This development build cannot update itself. Download a desktop installer to test the update flow.',
	comment: 'Desktop updater modal body for development or unpackaged builds.',
});
const MANUAL_UPDATE_BODY_DESCRIPTOR = msg({
	message:
		'This desktop build cannot install updates inside {productName}. Download the latest desktop installer instead.',
	comment:
		'Desktop updater modal body for platforms that do not support in-app installation. productName is the app name.',
});
const SYSTEM_MANAGED_UPDATE_BODY_DESCRIPTOR = msg({
	message: 'This install is managed by your system. Update {productName} from your software center or package manager.',
	comment: 'Desktop updater modal body for managed package builds such as Flatpak. productName is the app name.',
});
const OPEN_SOFTWARE_CENTER_DESCRIPTOR = msg({
	message: 'Open software center',
	comment: 'Button label that opens the software center via appstream protocol.',
});
const OPEN_DESKTOP_DOWNLOADS_DESCRIPTOR = msg({
	message: 'Open desktop downloads',
	comment: 'Button label that opens the Fluxer desktop downloads page in a browser.',
});
const DESKTOP_UPDATE_READY_DESCRIPTOR = msg({
	message: 'Desktop update ready',
	comment: 'Modal title shown when a desktop app update has finished downloading.',
});
const DESKTOP_VERSION_HAS_BEEN_DOWNLOADED_DESCRIPTOR = msg({
	message: 'Desktop version {version} has been downloaded. Restart {productName} to finish installing.',
	comment:
		'Desktop updater modal body. The version placeholder is the downloaded app version; productName is the app name.',
});
const THE_DESKTOP_UPDATE_HAS_BEEN_DOWNLOADED_DESCRIPTOR = msg({
	message: 'The desktop update has been downloaded. Restart {productName} to finish installing.',
	comment: 'Desktop updater modal body when the downloaded version is unknown. productName is the app name.',
});
const RESTART_FLUXER_DESCRIPTOR = msg({
	message: 'Restart {productName}',
	comment: 'Button label that restarts the app to apply a desktop update. productName is the app name.',
});
const UPDATE_CHECK_FAILED_DESCRIPTOR = msg({
	message: "Couldn't check for updates",
	comment: 'Modal title shown when a user-initiated update check fails.',
});
const UPDATE_CHECK_FAILED_BODY_DESCRIPTOR = msg({
	message: '{productName} could not reach the update service. Check your connection and try again.',
	comment: 'Modal body shown when a user-initiated update check fails. productName is the app name.',
});
const DESKTOP_UPDATE_DOWNLOAD_FAILED_DESCRIPTOR = msg({
	message: "Couldn't download the desktop update",
	comment: 'Modal title shown when a desktop update download fails.',
});
const DESKTOP_UPDATE_DOWNLOAD_FAILED_BODY_DESCRIPTOR = msg({
	message:
		'The desktop update could not be downloaded. Try again from the update button when your connection is stable.',
	comment: 'Modal body shown when a desktop update download fails.',
});
const DESKTOP_UPDATE_INSTALL_FAILED_DESCRIPTOR = msg({
	message: "Couldn't start the desktop update",
	comment: 'Modal title shown when starting installation of a downloaded desktop update fails.',
});
const DESKTOP_UPDATE_INSTALL_FAILED_BODY_DESCRIPTOR = msg({
	message: '{productName} could not restart into the downloaded update. Try again from the update button.',
	comment:
		'Modal body shown when starting installation of a downloaded desktop update fails. productName is the app name.',
});
const LINUX_PACKAGE_UPDATE_INTRO_DESCRIPTOR = msg({
	message:
		'A Linux desktop update is available. Choose the package format for this system and {productName} will save it locally.',
	comment:
		'Linux desktop updater modal body shown before version details and package format selector. productName is the app name.',
});
const INSTALLED_VERSION_LABEL_DESCRIPTOR = msg({
	message: 'Installed',
	comment: 'Label for the installed desktop version in the Linux update modal.',
});
const AVAILABLE_VERSION_LABEL_DESCRIPTOR = msg({
	message: 'Available',
	comment: 'Label for the available desktop version in the Linux update modal.',
});
const UNKNOWN_VERSION_DESCRIPTOR = msg({
	message: 'Unknown',
	comment: 'Fallback version label when the installed desktop version is unknown.',
});
const NEW_VERSION_DESCRIPTOR = msg({
	message: 'New version',
	comment: 'Fallback version label when the available desktop version is unknown.',
});
const LINUX_PACKAGE_LABEL_DESCRIPTOR = msg({
	message: 'Linux package',
	comment: 'Select label for choosing the Linux desktop package format to download.',
});
const PACKAGE_HELP_DESCRIPTOR = msg({
	message: 'The downloaded file is saved to your computer. Install it with your normal package manager.',
	comment: 'Help text below the Linux package format selector in the desktop updater modal.',
});

const UPDATE_AVAILABLE_KEY = 'updater-available';
const UP_TO_DATE_KEY = 'updater-up-to-date';
const UNSUPPORTED_KEY = 'updater-unsupported';
const ERROR_KEY = 'updater-error';

export function pushUpdateAvailableModal(version: string | null, onDownload: () => void | Promise<void>): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={i18n._(DESKTOP_UPDATE_AVAILABLE_DESCRIPTOR)}
				description={
					version
						? i18n._(DESKTOP_VERSION_IS_READY_TO_DOWNLOAD_DESCRIPTOR, {version, productName: PRODUCT_NAME})
						: i18n._(A_NEW_DESKTOP_VERSION_IS_READY_TO_DOWNLOAD_DESCRIPTOR, {productName: PRODUCT_NAME})
				}
				primaryText={i18n._(DOWNLOAD_INSTALLER_DESCRIPTOR)}
				secondaryText={i18n._(LATER_DESCRIPTOR)}
				onPrimary={async () => {
					await onDownload();
				}}
				data-flx="updater.updater-modal-commands.push-update-available-modal.confirm-modal"
			/>
		)),
		UPDATE_AVAILABLE_KEY,
	);
}

interface ManualUpdateAvailableModalProps {
	currentVersion: string | null;
	version: string | null;
	options: ReadonlyArray<UpdaterDownloadOption>;
	onDownload: (option: UpdaterDownloadOption) => void | Promise<void>;
}

function ManualUpdateAvailableModal({currentVersion, version, options, onDownload}: ManualUpdateAvailableModalProps) {
	const [selectedFormat, setSelectedFormat] = useState<UpdaterDownloadOption['format'] | null>(
		options[0]?.format ?? null,
	);
	const selectOptions = useMemo(
		() =>
			options.map((option) => ({
				value: option.format,
				label: option.label,
			})),
		[options],
	);
	const selectedOption = options.find((option) => option.format === selectedFormat) ?? options[0];
	return (
		<ConfirmModal
			title={i18n._(DESKTOP_UPDATE_AVAILABLE_DESCRIPTOR)}
			description={
				<div className={styles.manualUpdateBody} data-flx="updater.manual-update.body">
					<p className={styles.manualUpdateIntro} data-flx="updater.manual-update.intro">
						{i18n._(LINUX_PACKAGE_UPDATE_INTRO_DESCRIPTOR, {productName: PRODUCT_NAME})}
					</p>
					<dl className={styles.versionList} data-flx="updater.manual-update.version-list">
						<dt className={styles.versionLabel} data-flx="updater.manual-update.installed-label">
							{i18n._(INSTALLED_VERSION_LABEL_DESCRIPTOR)}
						</dt>
						<dd className={styles.versionValue} data-flx="updater.manual-update.installed-value">
							{currentVersion ?? i18n._(UNKNOWN_VERSION_DESCRIPTOR)}
						</dd>
						<dt className={styles.versionLabel} data-flx="updater.manual-update.available-label">
							{i18n._(AVAILABLE_VERSION_LABEL_DESCRIPTOR)}
						</dt>
						<dd className={styles.versionValue} data-flx="updater.manual-update.available-value">
							{version ?? i18n._(NEW_VERSION_DESCRIPTOR)}
						</dd>
					</dl>
					<Combobox
						className={styles.packageSelect}
						label={i18n._(LINUX_PACKAGE_LABEL_DESCRIPTOR)}
						value={selectedFormat}
						options={selectOptions}
						onChange={setSelectedFormat}
						isSearchable={false}
						density="compact"
						data-flx="updater.manual-update.package-select.change"
					/>
					<p className={styles.manualUpdateHelp} data-flx="updater.manual-update.help">
						{i18n._(PACKAGE_HELP_DESCRIPTOR)}
					</p>
				</div>
			}
			primaryText={i18n._(DOWNLOAD_PACKAGE_DESCRIPTOR)}
			secondaryText={i18n._(LATER_DESCRIPTOR)}
			onPrimary={async () => {
				if (selectedOption) {
					await onDownload(selectedOption);
				}
			}}
			data-flx="updater.updater-modal-commands.push-manual-update-available-modal.confirm-modal"
		/>
	);
}

export function pushManualUpdateAvailableModal(options: ManualUpdateAvailableModalProps): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ManualUpdateAvailableModal
				data-flx="updater.updater-modal-commands.push-manual-update-available-modal.manual-update-available-modal"
				{...options}
			/>
		)),
		UPDATE_AVAILABLE_KEY,
	);
}

export function pushUpToDateModal(currentVersion: string | null): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={i18n._(DESKTOP_APP_IS_UP_TO_DATE_DESCRIPTOR)}
				description={
					currentVersion
						? i18n._(INSTALLED_VERSION_IS_CURRENT_DESCRIPTOR, {currentVersion})
						: i18n._(NO_DESKTOP_UPDATE_IS_AVAILABLE_DESCRIPTOR)
				}
				secondaryText={i18n._(CLOSE_DESCRIPTOR)}
				data-flx="updater.updater-modal-commands.push-up-to-date-modal.confirm-modal"
			/>
		)),
		UP_TO_DATE_KEY,
	);
}

export function pushUnsupportedUpdateModal(
	reason: 'platform' | 'unpackaged' | 'managed-package',
	downloadUrl?: string | null,
): void {
	ModalCommands.pushWithKey(
		modal(() => {
			if (reason === 'managed-package') {
				return (
					<ConfirmModal
						title={i18n._(SYSTEM_MANAGED_INSTALL_DESCRIPTOR)}
						description={i18n._(SYSTEM_MANAGED_UPDATE_BODY_DESCRIPTOR, {productName: PRODUCT_NAME})}
						primaryText={i18n._(OPEN_SOFTWARE_CENTER_DESCRIPTOR)}
						secondaryText={i18n._(CLOSE_DESCRIPTOR)}
						onPrimary={() => {
							const appstreamUrl = isCanaryDesktop()
								? 'appstream://app.fluxer.FluxerCanary'
								: 'appstream://app.fluxer.Fluxer';

							void openExternalUrl(appstreamUrl);
						}}
						data-flx="updater.updater-modal-commands.push-unsupported-update-modal.confirm-modal"
					/>
				);
			}
			const resolvedDownloadUrl = downloadUrl ?? DESKTOP_DOWNLOAD_URL;
			return (
				<ConfirmModal
					title={
						reason === 'unpackaged' ? i18n._(DEVELOPMENT_BUILD_DESCRIPTOR) : i18n._(MANUAL_DESKTOP_UPDATE_DESCRIPTOR)
					}
					description={
						reason === 'unpackaged'
							? i18n._(DEVELOPMENT_BUILD_UPDATE_BODY_DESCRIPTOR)
							: i18n._(MANUAL_UPDATE_BODY_DESCRIPTOR, {productName: PRODUCT_NAME})
					}
					primaryText={i18n._(OPEN_DESKTOP_DOWNLOADS_DESCRIPTOR)}
					secondaryText={i18n._(LATER_DESCRIPTOR)}
					onPrimary={() => {
						void openExternalUrl(resolvedDownloadUrl);
					}}
					data-flx="updater.updater-modal-commands.push-unsupported-update-modal.confirm-modal"
				/>
			);
		}),
		UNSUPPORTED_KEY,
	);
}

export function pushUpdateReadyModal(version: string | null, onInstall: () => void | Promise<void>): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={i18n._(DESKTOP_UPDATE_READY_DESCRIPTOR)}
				description={
					version
						? i18n._(DESKTOP_VERSION_HAS_BEEN_DOWNLOADED_DESCRIPTOR, {version, productName: PRODUCT_NAME})
						: i18n._(THE_DESKTOP_UPDATE_HAS_BEEN_DOWNLOADED_DESCRIPTOR, {productName: PRODUCT_NAME})
				}
				primaryText={i18n._(RESTART_FLUXER_DESCRIPTOR, {productName: PRODUCT_NAME})}
				secondaryText={i18n._(LATER_DESCRIPTOR)}
				onPrimary={async () => {
					await onInstall();
				}}
				data-flx="updater.updater-modal-commands.push-update-ready-modal.confirm-modal"
			/>
		)),
		'updater-ready',
	);
}

function pushUpdaterErrorModal(getTitle: () => string, getDescription: () => string): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={getTitle()}
				description={getDescription()}
				secondaryText={i18n._(CLOSE_DESCRIPTOR)}
				data-flx="updater.updater-modal-commands.push-updater-error-modal.confirm-modal"
			/>
		)),
		ERROR_KEY,
	);
}

export function pushUpdateCheckFailedModal(): void {
	pushUpdaterErrorModal(
		() => i18n._(UPDATE_CHECK_FAILED_DESCRIPTOR),
		() => i18n._(UPDATE_CHECK_FAILED_BODY_DESCRIPTOR, {productName: PRODUCT_NAME}),
	);
}

export function pushDesktopUpdateDownloadFailedModal(): void {
	pushUpdaterErrorModal(
		() => i18n._(DESKTOP_UPDATE_DOWNLOAD_FAILED_DESCRIPTOR),
		() => i18n._(DESKTOP_UPDATE_DOWNLOAD_FAILED_BODY_DESCRIPTOR),
	);
}

export function pushDesktopUpdateInstallFailedModal(): void {
	pushUpdaterErrorModal(
		() => i18n._(DESKTOP_UPDATE_INSTALL_FAILED_DESCRIPTOR),
		() => i18n._(DESKTOP_UPDATE_INSTALL_FAILED_BODY_DESCRIPTOR, {productName: PRODUCT_NAME}),
	);
}
