// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {BLUESKY_PROVIDER_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as ConnectionCommands from '@app/features/connection/commands/ConnectionCommands';
import styles from '@app/features/connection/components/modals/AddConnectionModal.module.css';
import UserConnection from '@app/features/connection/state/UserConnection';
import {
	COPIED_DESCRIPTOR,
	DOMAIN_DESCRIPTOR,
	VERIFY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as FormUtils from '@app/lib/forms';
import {type ConnectionType, ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import type {ConnectionVerificationResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, ClipboardIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {useForm} from 'react-hook-form';

const YOU_ALREADY_HAVE_THIS_CONNECTION_DESCRIPTOR = msg({
	message: 'You already have this connection.',
	comment: 'Body text in the connection add connection modal.',
});
const COPY_HOST_DESCRIPTOR = msg({
	message: 'Copy host',
	comment: 'Short label in the connection add connection modal.',
});
const COPY_VALUE_DESCRIPTOR = msg({
	message: 'Copy value',
	comment: 'Short label in the connection add connection modal.',
});
const ADD_CONNECTION_FORM_DESCRIPTOR = msg({
	message: 'Add connection form',
	comment: 'Accessible form label in the connection add connection modal.',
});
const ADD_CONNECTION_DESCRIPTOR = msg({
	message: 'Add connection',
	comment: 'Short label in the connection add connection modal.',
});
const CONNECTION_TYPE_DESCRIPTOR = msg({
	message: 'Connection type',
	comment: 'Short label in the connection add connection modal.',
});
const HANDLE_DESCRIPTOR = msg({
	message: 'Handle',
	comment: 'Short label in the connection add connection modal.',
});
const CONNECT_WITH_PROVIDER_DESCRIPTOR = msg({
	message: 'Connect with {providerName}',
	comment: 'Button label for starting an external account connection. providerName is the provider display name.',
});
const VERIFY_CONNECTION_DESCRIPTOR = msg({
	message: 'Verify connection',
	comment: 'Short label in the connection add connection modal.',
});
const HOST_DESCRIPTOR = msg({
	message: 'Host',
	comment: 'Short label in the connection add connection modal.',
});
const VALUE_DESCRIPTOR = msg({
	message: 'Value',
	comment: 'Short label in the connection add connection modal.',
});
const COPY_RESET_DELAY_MS = 2000;

interface CopyButtonProps {
	copied: boolean;
	disabled?: boolean;
	label: string;
	onClick: () => void;
}

const CopyButton = ({copied, disabled = false, label, onClick}: CopyButtonProps) => (
	<button
		type="button"
		className={styles.copyButton}
		onClick={onClick}
		disabled={disabled}
		aria-label={label}
		data-flx="connection.add-connection-modal.copy-button.copy-button.click"
	>
		{copied ? (
			<CheckCircleIcon
				className={styles.copyIcon}
				size={remFromPx(16)}
				weight="bold"
				data-flx="connection.add-connection-modal.copy-button.copy-icon"
			/>
		) : (
			<ClipboardIcon
				className={styles.copyIcon}
				size={remFromPx(16)}
				data-flx="connection.add-connection-modal.copy-button.copy-icon--2"
			/>
		)}
	</button>
);

CopyButton.displayName = 'CopyButton';

interface InitiateFormInputs {
	identifier: string;
}

interface AddConnectionModalProps {
	defaultType?: ConnectionType;
}

type Step = 'initiate' | 'verify';

const STEP_ORDER: ReadonlyArray<Step> = ['initiate', 'verify'];
export const AddConnectionModal = observer(({defaultType}: AddConnectionModalProps) => {
	const {i18n} = useLingui();
	const [step, setStep] = useState<Step>('initiate');
	const [type, setType] = useState<ConnectionType>(
		defaultType ?? (RuntimeConfig.blueskyConnectionsEnabled ? ConnectionTypes.BLUESKY : ConnectionTypes.DOMAIN),
	);
	const [verificationData, setVerificationData] = useState<ConnectionVerificationResponse | null>(null);
	const [hostCopied, setHostCopied] = useState(false);
	const [valueCopied, setValueCopied] = useState(false);
	const pendingBlueskyHandle = useRef<string | null>(null);
	const initiateForm = useForm<InitiateFormInputs>();
	const [isVerifySubmitting, setIsVerifySubmitting] = useState(false);
	const connectionTypeOptions = useMemo(
		() => [
			...(RuntimeConfig.blueskyConnectionsEnabled
				? [{value: ConnectionTypes.BLUESKY, label: BLUESKY_PROVIDER_NAME}]
				: []),
			{value: ConnectionTypes.DOMAIN, label: i18n._(DOMAIN_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const handleTypeChange = useCallback((value: ConnectionType) => setType(value), []);
	const onSubmitInitiate = useCallback(
		async (data: InitiateFormInputs) => {
			let identifier = data.identifier.trim();
			if (type === ConnectionTypes.BLUESKY) {
				identifier = identifier.replace(/^https?:\/\/bsky\.app\/profile\//i, '').replace(/^@/, '');
			}
			if (UserConnection.hasConnectionByTypeAndName(type, identifier)) {
				initiateForm.setError('identifier', {
					type: 'validate',
					message: i18n._(YOU_ALREADY_HAVE_THIS_CONNECTION_DESCRIPTOR),
				});
				return;
			}
			if (type === ConnectionTypes.BLUESKY) {
				await ConnectionCommands.authorizeBlueskyConnection(i18n, identifier);
				pendingBlueskyHandle.current = identifier.toLowerCase();
				return;
			}
			const result = await ConnectionCommands.initiateConnection(i18n, type, identifier);
			setVerificationData(result);
			setStep('verify');
		},
		[i18n, initiateForm, type],
	);
	const handleVerifyConfirm = useCallback(async () => {
		if (!verificationData) return;
		setIsVerifySubmitting(true);
		try {
			await ConnectionCommands.verifyAndCreateConnection(i18n, verificationData.initiation_token);
			ModalCommands.popByType(AddConnectionModal);
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error);
		} finally {
			setIsVerifySubmitting(false);
		}
	}, [i18n, verificationData]);
	const {handleSubmit: handleInitiateSubmit} = useFormSubmit({
		form: initiateForm,
		onSubmit: onSubmitInitiate,
		defaultErrorField: 'identifier',
	});
	const hasBlueskyConnection = UserConnection.hasConnectionByTypeAndName(
		ConnectionTypes.BLUESKY,
		pendingBlueskyHandle.current ?? '',
	);
	useEffect(() => {
		if (pendingBlueskyHandle.current && hasBlueskyConnection) {
			ModalCommands.popByType(AddConnectionModal);
		}
	}, [hasBlueskyConnection]);
	const hostRecord = useMemo(
		() => (verificationData?.id ? `_fluxer.${verificationData.id}` : ''),
		[verificationData?.id],
	);
	const dnsValue = useMemo(
		() => (verificationData?.token ? `fluxer-verification=${verificationData.token}` : ''),
		[verificationData?.token],
	);
	const dnsUrl = useMemo(
		() => (verificationData?.id ? `https://${verificationData.id}/.well-known/fluxer-verification` : ''),
		[verificationData?.id],
	);
	const notifyAndReset = useCallback((setter: (value: boolean) => void) => {
		setter(true);
		setTimeout(() => setter(false), COPY_RESET_DELAY_MS);
	}, []);
	const handleCopyHost = useCallback(async () => {
		if (!hostRecord) return;
		const success = await TextCopyCommands.copy(i18n, hostRecord);
		if (success) {
			notifyAndReset(setHostCopied);
		}
	}, [hostRecord, i18n, notifyAndReset]);
	const handleCopyValue = useCallback(async () => {
		if (!dnsValue) return;
		const success = await TextCopyCommands.copy(i18n, dnsValue);
		if (success) {
			notifyAndReset(setValueCopied);
		}
	}, [dnsValue, i18n, notifyAndReset]);
	const downloadTokenFile = useCallback(() => {
		if (!verificationData?.token) return;
		const blob = new Blob([verificationData.token], {type: 'text/plain'});
		const blobUrl = URL.createObjectURL(blob);
		const link = document.createElement('a');
		link.href = blobUrl;
		link.download = 'fluxer-verification';
		document.body.appendChild(link);
		link.click();
		document.body.removeChild(link);
		URL.revokeObjectURL(blobUrl);
	}, [verificationData?.token]);
	const hostCopyLabel = hostCopied ? i18n._(COPIED_DESCRIPTOR) : i18n._(COPY_HOST_DESCRIPTOR);
	const valueCopyLabel = valueCopied ? i18n._(COPIED_DESCRIPTOR) : i18n._(COPY_VALUE_DESCRIPTOR);
	const renderInitiateStep = () => (
		<Form
			form={initiateForm}
			onSubmit={handleInitiateSubmit}
			aria-label={i18n._(ADD_CONNECTION_FORM_DESCRIPTOR)}
			data-flx="connection.add-connection-modal.form.initiate-submit"
		>
			<div className={styles.stack} data-flx="connection.add-connection-modal.stack">
				<Combobox
					label={i18n._(CONNECTION_TYPE_DESCRIPTOR)}
					value={type}
					options={connectionTypeOptions}
					onChange={handleTypeChange}
					data-flx="connection.add-connection-modal.select.type-change"
				/>
				<Input
					data-flx="connection.add-connection-modal.input"
					{...initiateForm.register('identifier', {required: true})}
					autoFocus={true}
					error={initiateForm.formState.errors.identifier?.message}
					label={type === ConnectionTypes.BLUESKY ? i18n._(HANDLE_DESCRIPTOR) : i18n._(DOMAIN_DESCRIPTOR)}
					placeholder={type === ConnectionTypes.BLUESKY ? 'username.bsky.social' : 'example.com'}
					required={true}
				/>
			</div>
		</Form>
	);
	const renderVerifyStep = () => (
		<div className={styles.stack} data-flx="connection.add-connection-modal.stack--2">
			<p className={styles.instructions} data-flx="connection.add-connection-modal.instructions">
				<Trans>Choose one of the methods below to prove domain ownership. You only need to complete one.</Trans>
			</p>
			<div className={styles.dnsCard} data-flx="connection.add-connection-modal.dns-card">
				<div className={styles.dnsHeading} data-flx="connection.add-connection-modal.dns-heading">
					<p className={styles.dnsTitle} data-flx="connection.add-connection-modal.dns-title">
						<Trans>DNS TXT record</Trans>
					</p>
				</div>
				<div className={styles.dnsFields} data-flx="connection.add-connection-modal.dns-fields">
					<Input
						label={i18n._(HOST_DESCRIPTOR)}
						value={hostRecord}
						readOnly={true}
						className={styles.dnsInput}
						rightElement={
							<CopyButton
								onClick={handleCopyHost}
								copied={hostCopied}
								disabled={!hostRecord}
								label={hostCopyLabel}
								data-flx="connection.add-connection-modal.copy-button.copy-host"
							/>
						}
						data-flx="connection.add-connection-modal.dns-input"
					/>
					<Input
						label={i18n._(VALUE_DESCRIPTOR)}
						value={dnsValue}
						readOnly={true}
						className={styles.dnsInput}
						rightElement={
							<CopyButton
								onClick={handleCopyValue}
								copied={valueCopied}
								disabled={!dnsValue}
								label={valueCopyLabel}
								data-flx="connection.add-connection-modal.copy-button.copy-value"
							/>
						}
						data-flx="connection.add-connection-modal.dns-input--2"
					/>
				</div>
			</div>
			{dnsUrl && (
				<div className={styles.orDivider} data-flx="connection.add-connection-modal.or-divider">
					<div className={styles.orDividerLine} data-flx="connection.add-connection-modal.or-divider-line" />
					<span className={styles.orDividerText} data-flx="connection.add-connection-modal.or-divider-text">
						<Trans>or</Trans>
					</span>
					<div className={styles.orDividerLine} data-flx="connection.add-connection-modal.or-divider-line--2" />
				</div>
			)}
			{dnsUrl && (
				<div className={styles.tokenCard} data-flx="connection.add-connection-modal.token-card">
					<div className={styles.tokenCardHeader} data-flx="connection.add-connection-modal.token-card-header">
						<p className={styles.tokenTitle} data-flx="connection.add-connection-modal.token-title">
							<Trans>Serve the token file</Trans>
						</p>
						<p className={styles.tokenSubtitle} data-flx="connection.add-connection-modal.token-subtitle">
							<Trans>
								Download{' '}
								<code className={styles.inlineCode} data-flx="connection.add-connection-modal.inline-code">
									fluxer-verification
								</code>{' '}
								and place it in your{' '}
								<code className={styles.inlineCode} data-flx="connection.add-connection-modal.inline-code--2">
									.well-known
								</code>{' '}
								folder so we can validate the domain.
							</Trans>
						</p>
					</div>
					<div className={styles.tokenDownloadRow} data-flx="connection.add-connection-modal.token-download-row">
						<Button
							type="button"
							variant="secondary"
							compact
							onClick={downloadTokenFile}
							disabled={!verificationData?.token}
							data-flx="connection.add-connection-modal.button.download-token-file"
						>
							<Trans>Download fluxer-verification</Trans>
						</Button>
					</div>
					<p className={styles.tokenMeta} data-flx="connection.add-connection-modal.token-meta">
						<Trans>
							The file contains the verification token we will fetch from{' '}
							<code className={styles.inlineCode} data-flx="connection.add-connection-modal.inline-code--3">
								{dnsUrl}
							</code>
							.
						</Trans>
					</p>
				</div>
			)}
		</div>
	);
	const renderStepBody = () => (step === 'initiate' ? renderInitiateStep() : renderVerifyStep());
	const renderStepFooter = () => {
		if (step === 'initiate') {
			return (
				<>
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="connection.add-connection-modal.button.pop">
						<Trans>Cancel</Trans>
					</Button>
					<Button
						onClick={handleInitiateSubmit}
						submitting={initiateForm.formState.isSubmitting}
						data-flx="connection.add-connection-modal.button.submit"
					>
						{type === ConnectionTypes.BLUESKY ? (
							i18n._(CONNECT_WITH_PROVIDER_DESCRIPTOR, {providerName: BLUESKY_PROVIDER_NAME})
						) : (
							<Trans>Continue</Trans>
						)}
					</Button>
				</>
			);
		}
		return (
			<>
				<Button
					onClick={() => setStep('initiate')}
					variant="secondary"
					data-flx="connection.add-connection-modal.button.set-step"
				>
					<Trans>Back</Trans>
				</Button>
				<Button
					onClick={handleVerifyConfirm}
					submitting={isVerifySubmitting}
					data-flx="connection.add-connection-modal.button.verify-confirm"
				>
					{i18n._(VERIFY_DESCRIPTOR)}
				</Button>
			</>
		);
	};
	return (
		<Modal.Root size="small" centered data-flx="connection.add-connection-modal.modal-root">
			<Modal.Header
				title={step === 'initiate' ? i18n._(ADD_CONNECTION_DESCRIPTOR) : i18n._(VERIFY_CONNECTION_DESCRIPTOR)}
				data-flx="connection.add-connection-modal.modal-header"
			/>
			<Modal.Content contentClassName={styles.content} data-flx="connection.add-connection-modal.modal-content">
				<SteppedCarousel step={step} steps={STEP_ORDER} data-flx="connection.add-connection-modal.stepped-carousel">
					{renderStepBody()}
				</SteppedCarousel>
			</Modal.Content>
			<Modal.Footer data-flx="connection.add-connection-modal.modal-footer">{renderStepFooter()}</Modal.Footer>
		</Modal.Root>
	);
});
