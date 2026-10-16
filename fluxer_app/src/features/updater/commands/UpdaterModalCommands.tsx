// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {DESKTOP_DOWNLOAD_URL} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {isCanaryDesktop, openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

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
const DESKTOP_UPDATE_RESTARTS_APP_DESCRIPTOR = msg({
	message: '{productName} closes, installs the update and opens again.',
	comment:
		'Desktop updater modal body after a manual update check finds an update. Confirming closes the app window and opens the updater. productName is the app name.',
});
const UPDATE_NOW_DESCRIPTOR = msg({
	message: 'Update now',
	comment: 'Button label that closes the desktop app and opens the updater to install an available update.',
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
const MANAGED_SHELL_UPDATE_FLATPAK_BODY_DESCRIPTOR = msg({
	message: '{productName} {version} is available. Update it from your software center.',
	comment:
		'Desktop updater modal body for a Flatpak install when a newer desktop version exists. The version placeholder is the new app version and productName is the app name.',
});
const MANAGED_SHELL_UPDATE_PACKAGE_BODY_DESCRIPTOR = msg({
	message: '{productName} {version} is available. Update it with your package manager, or download the new version.',
	comment:
		'Desktop updater modal body for a Linux install managed by a package manager, such as a deb, rpm or AUR package, when a newer desktop version exists. The version placeholder is the new app version and productName is the app name.',
});
const OPEN_SOFTWARE_CENTER_DESCRIPTOR = msg({
	message: 'Open software center',
	comment: 'Button label that opens the software center via appstream protocol.',
});
const OPEN_DESKTOP_DOWNLOADS_DESCRIPTOR = msg({
	message: 'Open desktop downloads',
	comment: 'Button label that opens the Fluxer desktop downloads page in a browser.',
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
const UPDATE_AVAILABLE_KEY = 'updater-available';
const UP_TO_DATE_KEY = 'updater-up-to-date';
const UNSUPPORTED_KEY = 'updater-unsupported';
const ERROR_KEY = 'updater-error';

function openSoftwareCenter(appId: string = isCanaryDesktop() ? 'app.fluxer.FluxerCanary' : 'app.fluxer.Fluxer'): void {
	void openExternalUrl(`appstream://${appId}`);
}

function managedShellUpdateBody(version: string | null, flatpakAppId: string | null): string {
	const productName = RuntimeConfig.productName;
	if (version == null) {
		return i18n._(SYSTEM_MANAGED_UPDATE_BODY_DESCRIPTOR, {productName});
	}
	return flatpakAppId != null
		? i18n._(MANAGED_SHELL_UPDATE_FLATPAK_BODY_DESCRIPTOR, {productName, version})
		: i18n._(MANAGED_SHELL_UPDATE_PACKAGE_BODY_DESCRIPTOR, {productName, version});
}

export function pushUpdateAvailableModal(version: string | null, onDownload: () => void | Promise<void>): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={i18n._(DESKTOP_UPDATE_AVAILABLE_DESCRIPTOR)}
				description={
					version
						? i18n._(DESKTOP_VERSION_IS_READY_TO_DOWNLOAD_DESCRIPTOR, {version, productName: RuntimeConfig.productName})
						: i18n._(A_NEW_DESKTOP_VERSION_IS_READY_TO_DOWNLOAD_DESCRIPTOR, {productName: RuntimeConfig.productName})
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

export function pushManagedShellUpdateModal({
	version,
	flatpakAppId,
}: {
	version: string | null;
	flatpakAppId: string | null;
}): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={i18n._(DESKTOP_UPDATE_AVAILABLE_DESCRIPTOR)}
				description={managedShellUpdateBody(version, flatpakAppId)}
				primaryText={
					flatpakAppId != null ? i18n._(OPEN_SOFTWARE_CENTER_DESCRIPTOR) : i18n._(OPEN_DESKTOP_DOWNLOADS_DESCRIPTOR)
				}
				secondaryText={i18n._(LATER_DESCRIPTOR)}
				onPrimary={() => {
					if (flatpakAppId != null) {
						openSoftwareCenter(flatpakAppId);
						return;
					}
					void openExternalUrl(DESKTOP_DOWNLOAD_URL);
				}}
				data-flx="updater.updater-modal-commands.push-managed-shell-update-modal.confirm-modal"
			/>
		)),
		UPDATE_AVAILABLE_KEY,
	);
}

export function pushDesktopUpdateAvailableModal(onUpdate: () => void | Promise<void>): void {
	ModalCommands.pushWithKey(
		modal(() => (
			<ConfirmModal
				title={i18n._(DESKTOP_UPDATE_AVAILABLE_DESCRIPTOR)}
				description={i18n._(DESKTOP_UPDATE_RESTARTS_APP_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				primaryText={i18n._(UPDATE_NOW_DESCRIPTOR)}
				secondaryText={i18n._(LATER_DESCRIPTOR)}
				onPrimary={async () => {
					await onUpdate();
				}}
				data-flx="updater.updater-modal-commands.push-desktop-update-available-modal.confirm-modal"
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
						description={i18n._(SYSTEM_MANAGED_UPDATE_BODY_DESCRIPTOR, {productName: RuntimeConfig.productName})}
						primaryText={i18n._(OPEN_SOFTWARE_CENTER_DESCRIPTOR)}
						secondaryText={i18n._(CLOSE_DESCRIPTOR)}
						onPrimary={() => openSoftwareCenter()}
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
							: i18n._(MANUAL_UPDATE_BODY_DESCRIPTOR, {productName: RuntimeConfig.productName})
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
		() => i18n._(UPDATE_CHECK_FAILED_BODY_DESCRIPTOR, {productName: RuntimeConfig.productName}),
	);
}

export function pushDesktopUpdateDownloadFailedModal(): void {
	pushUpdaterErrorModal(
		() => i18n._(DESKTOP_UPDATE_DOWNLOAD_FAILED_DESCRIPTOR),
		() => i18n._(DESKTOP_UPDATE_DOWNLOAD_FAILED_BODY_DESCRIPTOR),
	);
}
