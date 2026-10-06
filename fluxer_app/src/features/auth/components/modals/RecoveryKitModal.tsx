// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as MfaCommands from '@app/features/auth/commands/MfaCommands';
import styles from '@app/features/auth/components/modals/RecoveryKitModal.module.css';
import {isAbortError} from '@app/features/auth/state/SudoPrompt';
import {
	buildRecoveryKitPdf,
	createRecoveryKitQrDataUrl,
	type RecoveryKitSheet,
} from '@app/features/auth/utils/RecoveryKitSheet';
import {USERNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {TimestampStyle} from '@app/features/messaging/utils/markdown/parser/Enums';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {downloadBlob} from '@app/features/platform/utils/DownloadFile';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {shouldUse12HourFormat} from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {formatUserTag} from '@app/features/user/utils/UserTagUtils';
import {RECOVERY_KEY_SEPARATOR} from '@fluxer/constants/src/RecoveryKeyUtils';
import {formatTimestampWithStyle} from '@fluxer/date_utils/src/DateTimestampStyle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ClipboardIcon, DownloadIcon, PrinterIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {createPortal} from 'react-dom';

const logger = new Logger('RecoveryKitModal');

const RECOVERY_KIT_MODAL_KEY = 'recovery-kit-modal';

const YOUR_RECOVERY_KIT_DESCRIPTOR = msg({
	message: 'Your recovery kit',
	comment: 'Title of the modal that shows a newly created account recovery kit.',
});
const RECOVERY_KIT_CREATED_DESCRIPTOR = msg({
	message:
		'If you forget your password, this kit is the only way back into your account. Store it somewhere safe, like a password manager or a printed copy.',
	comment: 'Recovery kit modal description shown after a new recovery kit is created.',
});
const RECOVERY_KIT_REPLACED_DESCRIPTOR = msg({
	message: 'Your old recovery kit no longer works. Store this new one somewhere safe.',
	comment: 'Recovery kit modal description shown after a recovery kit replaces an older one.',
});
const RECOVERY_KIT_RECOVERED_DESCRIPTOR = msg({
	message: 'Your password is reset. Your old recovery kit no longer works. Store this new one somewhere safe.',
	comment: 'Recovery kit modal description shown after someone recovers their account with a recovery key.',
});
const RECOVERY_KIT_WARNING_DESCRIPTOR = msg({
	message: 'Anyone with this key can reset your password. Never share it.',
	comment: 'Warning under the recovery key in the recovery kit modal.',
});
const RECOVERY_KEY_DESCRIPTOR = msg({
	message: 'Recovery key',
	comment: 'Label for the account recovery key in the recovery kit modal and the printed kit.',
});
const RECOVERY_KIT_DOCUMENT_TITLE_DESCRIPTOR = msg({
	message: '{productName} recovery kit',
	comment: 'Title of the printed or downloaded recovery kit. productName is the app name.',
});
const RECOVERY_KIT_DOCUMENT_INTRO_DESCRIPTOR = msg({
	message:
		'Keep this page somewhere safe. If you forget your password, you can use it to get back into your account. Anyone with this recovery key can reset your password, so never share it.',
	comment: 'Introduction at the top of the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_INSTANCE_DESCRIPTOR = msg({
	message: 'Instance',
	comment: 'Label for the server address in the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_CREATED_AT_DESCRIPTOR = msg({
	message: 'Created',
	comment: 'Label for the creation date in the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_QR_CAPTION_DESCRIPTOR = msg({
	message: 'Scan to open the recovery page with your details filled in.',
	comment: 'Caption under the QR code in the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_STEPS_TITLE_DESCRIPTOR = msg({
	message: 'How to recover your account',
	comment: 'Heading above the recovery steps in the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_STEP_OPEN_DESCRIPTOR = msg({
	message: 'Go to {recoverUrl} or scan the QR code.',
	comment: 'First recovery step in the printed recovery kit. recoverUrl is the address of the recovery page.',
});
const RECOVERY_KIT_STEP_ENTER_DESCRIPTOR = msg({
	message: 'Enter your username and this recovery key.',
	comment: 'Second recovery step in the printed recovery kit.',
});
const RECOVERY_KIT_STEP_PASSWORD_DESCRIPTOR = msg({
	message: 'Choose a new password. You get a new recovery kit and this one stops working.',
	comment: 'Third recovery step in the printed recovery kit.',
});
const RECOVERY_KIT_BACKUP_CODES_TITLE_DESCRIPTOR = msg({
	message: 'Two-factor backup codes',
	comment: 'Heading above the two-factor backup codes in the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_BACKUP_CODES_NOTE_DESCRIPTOR = msg({
	message: 'Each code works once in place of your authenticator app.',
	comment: 'Note under the two-factor backup codes heading in the printed or downloaded recovery kit.',
});
const RECOVERY_KIT_PDF_FAILED_DESCRIPTOR = msg({
	message: "Couldn't create the PDF. Try again or print the kit instead.",
	comment: 'Toast shown when building the recovery kit PDF fails.',
});
const RECOVERY_KIT_BACKUP_CODES_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load your backup codes.",
	comment: 'Toast shown when the two-factor backup codes for the recovery kit cannot be loaded.',
});

export type RecoveryKitModalReason = 'created' | 'replaced' | 'recovered';

export interface RecoveryKitModalProps {
	recoveryKey: string;
	createdAt: string;
	username?: string;
	discriminator?: string;
	reason?: RecoveryKitModalReason;
	onDone?: () => void;
}

function fileSlug(value: string): string {
	return value
		.toLowerCase()
		.replace(/[^a-z0-9]+/g, '-')
		.replace(/^-+|-+$/g, '');
}

const RecoveryKitPrintView = ({sheet, qrDataUrl}: {sheet: RecoveryKitSheet; qrDataUrl: string | null}) =>
	createPortal(
		<div className={styles.printSheet} data-flx="auth.recovery-kit-modal.print-sheet">
			<h1 className={styles.printTitle} data-flx="auth.recovery-kit-modal.print-view.print-title">
				{sheet.title}
			</h1>
			<p className={styles.printIntro} data-flx="auth.recovery-kit-modal.print-view.print-intro">
				{sheet.intro}
			</p>
			<div className={styles.printTop} data-flx="auth.recovery-kit-modal.print-view.print-top">
				<dl className={styles.printDetails} data-flx="auth.recovery-kit-modal.print-view.print-details">
					{sheet.details.map((detail) => (
						<div key={detail.label} data-flx="auth.recovery-kit-modal.print-view.div">
							<dt className={styles.printDetailLabel} data-flx="auth.recovery-kit-modal.print-view.print-detail-label">
								{detail.label}
							</dt>
							<dd className={styles.printDetailValue} data-flx="auth.recovery-kit-modal.print-view.print-detail-value">
								{detail.value}
							</dd>
						</div>
					))}
				</dl>
				{qrDataUrl ? (
					<div className={styles.printQr} data-flx="auth.recovery-kit-modal.print-view.print-qr">
						<img
							className={styles.printQrImage}
							src={qrDataUrl}
							alt=""
							data-flx="auth.recovery-kit-modal.print-view.print-qr-image"
						/>
						<span data-flx="auth.recovery-kit-modal.print-view.span">{sheet.qrCaption}</span>
					</div>
				) : null}
			</div>
			<h2 className={styles.printSectionTitle} data-flx="auth.recovery-kit-modal.print-view.print-section-title">
				{sheet.keyLabel}
			</h2>
			<div className={styles.printKey} data-flx="auth.recovery-kit-modal.print-view.print-key">
				{sheet.recoveryKey}
			</div>
			<h2 className={styles.printSectionTitle} data-flx="auth.recovery-kit-modal.print-view.print-section-title--2">
				{sheet.stepsTitle}
			</h2>
			<ol className={styles.printSteps} data-flx="auth.recovery-kit-modal.print-view.print-steps">
				{sheet.steps.map((step) => (
					<li key={step} data-flx="auth.recovery-kit-modal.print-view.li">
						{step}
					</li>
				))}
			</ol>
			{sheet.backupCodes && sheet.backupCodes.codes.length > 0 ? (
				<>
					<h2 className={styles.printSectionTitle} data-flx="auth.recovery-kit-modal.print-view.print-section-title--3">
						{sheet.backupCodes.title}
					</h2>
					<p className={styles.printIntro} data-flx="auth.recovery-kit-modal.print-view.print-intro--2">
						{sheet.backupCodes.note}
					</p>
					<div className={styles.printCodes} data-flx="auth.recovery-kit-modal.print-view.print-codes">
						{sheet.backupCodes.codes.map((code) => (
							<span key={code} data-flx="auth.recovery-kit-modal.print-view.span--2">
								{code}
							</span>
						))}
					</div>
				</>
			) : null}
		</div>,
		document.body,
	);

export const RecoveryKitModal = observer(
	({recoveryKey, createdAt, username, discriminator, reason = 'created', onDone}: RecoveryKitModalProps) => {
		const {i18n} = useLingui();
		const currentUser = Users.currentUser;
		const accountUsername = username ?? currentUser?.username ?? '';
		const accountDiscriminator = username ? discriminator : (discriminator ?? currentUser?.discriminator);
		const accountTag = accountDiscriminator
			? formatUserTag({username: accountUsername, discriminator: accountDiscriminator})
			: accountUsername;
		const canIncludeBackupCodes = Boolean(currentUser?.mfaEnabled) && (!username || username === currentUser?.username);
		const [acknowledged, setAcknowledged] = useState(false);
		const [includeBackupCodes, setIncludeBackupCodes] = useState(false);
		const [backupCodes, setBackupCodes] = useState<ReadonlyArray<string> | null>(null);
		const [loadingBackupCodes, setLoadingBackupCodes] = useState(false);
		const [buildingPdf, setBuildingPdf] = useState(false);
		const [qrDataUrl, setQrDataUrl] = useState<string | null>(null);
		const [qrFailed, setQrFailed] = useState(false);
		const [printing, setPrinting] = useState(false);
		const keyGroups = useMemo(() => recoveryKey.split(RECOVERY_KEY_SEPARATOR), [recoveryKey]);
		const recoverPageUrl = `${RuntimeConfig.webAppBaseUrl}${Routes.RECOVER_ACCOUNT}`;
		const recoverUrl = `${recoverPageUrl}#username=${encodeURIComponent(accountTag)}&key=${encodeURIComponent(recoveryKey)}`;
		const locale = getCurrentLocale();
		const timeFormat = UserSettings.getTimeFormat();
		const sheet = useMemo<RecoveryKitSheet>(() => {
			const productName = RuntimeConfig.productName;
			const createdDate = new Date(createdAt);
			const createdLabel = Number.isNaN(createdDate.getTime())
				? createdAt
				: formatTimestampWithStyle(
						Math.floor(createdDate.getTime() / 1000),
						TimestampStyle.ShortDateTime,
						locale,
						shouldUse12HourFormat(locale),
					);
			return {
				productName,
				title: i18n._(RECOVERY_KIT_DOCUMENT_TITLE_DESCRIPTOR, {productName}),
				intro: i18n._(RECOVERY_KIT_DOCUMENT_INTRO_DESCRIPTOR),
				details: [
					{label: i18n._(RECOVERY_KIT_INSTANCE_DESCRIPTOR), value: RuntimeConfig.webAppBaseUrl},
					{label: i18n._(USERNAME_DESCRIPTOR), value: accountTag},
					{label: i18n._(RECOVERY_KIT_CREATED_AT_DESCRIPTOR), value: createdLabel},
				],
				keyLabel: i18n._(RECOVERY_KEY_DESCRIPTOR),
				recoveryKey,
				recoverUrl,
				qrCaption: i18n._(RECOVERY_KIT_QR_CAPTION_DESCRIPTOR),
				stepsTitle: i18n._(RECOVERY_KIT_STEPS_TITLE_DESCRIPTOR),
				steps: [
					i18n._(RECOVERY_KIT_STEP_OPEN_DESCRIPTOR, {recoverUrl: recoverPageUrl}),
					i18n._(RECOVERY_KIT_STEP_ENTER_DESCRIPTOR),
					i18n._(RECOVERY_KIT_STEP_PASSWORD_DESCRIPTOR),
				],
				backupCodes:
					includeBackupCodes && backupCodes
						? {
								title: i18n._(RECOVERY_KIT_BACKUP_CODES_TITLE_DESCRIPTOR),
								note: i18n._(RECOVERY_KIT_BACKUP_CODES_NOTE_DESCRIPTOR),
								codes: backupCodes,
							}
						: null,
			};
		}, [
			accountTag,
			backupCodes,
			createdAt,
			i18n,
			i18n.locale,
			includeBackupCodes,
			locale,
			recoverPageUrl,
			recoverUrl,
			recoveryKey,
			timeFormat,
		]);
		useEffect(() => {
			let cancelled = false;
			createRecoveryKitQrDataUrl(recoverUrl)
				.then((dataUrl) => {
					if (!cancelled) setQrDataUrl(dataUrl);
				})
				.catch((error) => {
					logger.error('Failed to render recovery kit QR code', error);
					if (!cancelled) setQrFailed(true);
				});
			return () => {
				cancelled = true;
			};
		}, [recoverUrl]);
		useEffect(() => {
			if (!printing) return;
			const printingClass = styles.printing;
			document.body.classList.add(printingClass);
			const finish = () => setPrinting(false);
			window.addEventListener('afterprint', finish);
			const frame = window.requestAnimationFrame(() => window.print());
			return () => {
				window.cancelAnimationFrame(frame);
				window.removeEventListener('afterprint', finish);
				document.body.classList.remove(printingClass);
			};
		}, [printing]);
		const handleIncludeBackupCodes = useCallback(
			async (checked: boolean) => {
				setIncludeBackupCodes(checked);
				if (!checked || backupCodes) return;
				setLoadingBackupCodes(true);
				try {
					const codes = await MfaCommands.getBackupCodes(false);
					setBackupCodes(codes.filter(({consumed}) => !consumed).map(({code}) => code));
				} catch (error) {
					setIncludeBackupCodes(false);
					if (!isAbortError(error)) {
						ToastCommands.error(i18n._(RECOVERY_KIT_BACKUP_CODES_FAILED_DESCRIPTOR));
					}
				} finally {
					setLoadingBackupCodes(false);
				}
			},
			[backupCodes, i18n],
		);
		const handleDownload = useCallback(async () => {
			setBuildingPdf(true);
			try {
				const bytes = await buildRecoveryKitPdf(sheet);
				const fileName = [fileSlug(sheet.productName), 'recovery-kit', fileSlug(accountUsername)]
					.filter(Boolean)
					.join('-');
				downloadBlob(new Blob([bytes.slice().buffer], {type: 'application/pdf'}), `${fileName}.pdf`);
			} catch (error) {
				logger.error('Failed to build recovery kit PDF', error);
				ToastCommands.error(i18n._(RECOVERY_KIT_PDF_FAILED_DESCRIPTOR));
			} finally {
				setBuildingPdf(false);
			}
		}, [accountUsername, i18n, sheet]);
		const handleDone = useCallback(() => {
			if (!acknowledged) return;
			ModalCommands.popWithKey(RECOVERY_KIT_MODAL_KEY);
			onDone?.();
		}, [acknowledged, onDone]);
		const description =
			reason === 'recovered'
				? i18n._(RECOVERY_KIT_RECOVERED_DESCRIPTOR)
				: reason === 'replaced'
					? i18n._(RECOVERY_KIT_REPLACED_DESCRIPTOR)
					: i18n._(RECOVERY_KIT_CREATED_DESCRIPTOR);
		const optionsBusy = loadingBackupCodes || buildingPdf;
		return (
			<Modal.Root size="small" centered onClose={handleDone} data-flx="auth.recovery-kit-modal.modal-root">
				<Modal.Header
					title={i18n._(YOUR_RECOVERY_KIT_DESCRIPTOR)}
					hideCloseButton
					data-flx="auth.recovery-kit-modal.modal-header"
				/>
				<Modal.Content data-flx="auth.recovery-kit-modal.modal-content">
					<Modal.ContentLayout data-flx="auth.recovery-kit-modal.modal-content-layout">
						<Modal.Description data-flx="auth.recovery-kit-modal.description">{description}</Modal.Description>
						<div
							className={styles.keyGrid}
							role="group"
							aria-label={i18n._(RECOVERY_KEY_DESCRIPTOR)}
							data-flx="auth.recovery-kit-modal.key-grid"
						>
							{keyGroups.map((group, index) => (
								<span
									key={`${index}-${group}`}
									className={styles.keyGroup}
									data-flx="auth.recovery-kit-modal.key-group"
								>
									{group}
								</span>
							))}
						</div>
						<p className={styles.warning} data-flx="auth.recovery-kit-modal.warning">
							{i18n._(RECOVERY_KIT_WARNING_DESCRIPTOR)}
						</p>
						<div className={styles.buttonRow} data-flx="auth.recovery-kit-modal.button-row">
							<Button
								small={true}
								leftIcon={
									<DownloadIcon className={styles.buttonIcon} data-flx="auth.recovery-kit-modal.download-icon" />
								}
								submitting={buildingPdf}
								disabled={loadingBackupCodes}
								onClick={handleDownload}
								data-flx="auth.recovery-kit-modal.button.download"
							>
								<Trans comment="Button that downloads the account recovery kit as a PDF file.">Download PDF</Trans>
							</Button>
							<Button
								variant="secondary"
								small={true}
								leftIcon={<PrinterIcon className={styles.buttonIcon} data-flx="auth.recovery-kit-modal.print-icon" />}
								disabled={optionsBusy || (qrDataUrl === null && !qrFailed)}
								onClick={() => setPrinting(true)}
								data-flx="auth.recovery-kit-modal.button.print"
							>
								<Trans comment="Button that prints the account recovery kit.">Print</Trans>
							</Button>
							<Button
								variant="secondary"
								small={true}
								leftIcon={<ClipboardIcon className={styles.buttonIcon} data-flx="auth.recovery-kit-modal.copy-icon" />}
								onClick={() => TextCopyCommands.copy(i18n, recoveryKey)}
								data-flx="auth.recovery-kit-modal.button.copy"
							>
								<Trans comment="Button that copies the account recovery key to the clipboard.">Copy</Trans>
							</Button>
						</div>
						<div className={styles.options} data-flx="auth.recovery-kit-modal.options">
							{canIncludeBackupCodes ? (
								<Checkbox
									checked={includeBackupCodes}
									disabled={optionsBusy}
									onChange={handleIncludeBackupCodes}
									data-flx="auth.recovery-kit-modal.checkbox.include-backup-codes"
								>
									<span className={styles.optionLabel} data-flx="auth.recovery-kit-modal.include-backup-codes-label">
										<Trans>Include my two-factor backup codes in the PDF and printout</Trans>
									</span>
								</Checkbox>
							) : null}
							<Checkbox
								checked={acknowledged}
								onChange={setAcknowledged}
								data-flx="auth.recovery-kit-modal.checkbox.acknowledge"
							>
								<span className={styles.optionLabel} data-flx="auth.recovery-kit-modal.acknowledge-label">
									<Trans>I've stored my recovery kit somewhere safe</Trans>
								</span>
							</Checkbox>
						</div>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.recovery-kit-modal.footer">
					<Button disabled={!acknowledged} onClick={handleDone} data-flx="auth.recovery-kit-modal.button.done">
						<Trans comment="Button that closes the recovery kit modal once the kit is stored.">Done</Trans>
					</Button>
				</Modal.Footer>
				{printing ? (
					<RecoveryKitPrintView sheet={sheet} qrDataUrl={qrDataUrl} data-flx="auth.recovery-kit-modal.print-view" />
				) : null}
			</Modal.Root>
		);
	},
);

export function openRecoveryKitModal(props: RecoveryKitModalProps): void {
	ModalCommands.popWithKey(RECOVERY_KIT_MODAL_KEY);
	ModalCommands.pushWithKey(
		modal(() => <RecoveryKitModal {...props} data-flx="auth.recovery-kit-modal.open-recovery-kit-modal" />),
		RECOVERY_KIT_MODAL_KEY,
	);
}
