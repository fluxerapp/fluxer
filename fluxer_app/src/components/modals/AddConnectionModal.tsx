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

import * as ConnectionActionCreators from '@app/actions/ConnectionActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import * as TextCopyActionCreators from '@app/actions/TextCopyActionCreators';
import {Form} from '@app/components/form/Form';
import {FormErrorText} from '@app/components/form/FormErrorText';
import {Input} from '@app/components/form/Input';
import {Select} from '@app/components/form/Select';
import styles from '@app/components/modals/AddConnectionModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import UserConnectionStore from '@app/stores/UserConnectionStore';
import {type ConnectionType, ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import type {ConnectionVerificationResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, ClipboardIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {useForm} from 'react-hook-form';

const COPY_RESET_DELAY_MS = 2000;

interface CopyButtonProps {
	copied: boolean;
	disabled?: boolean;
	label: string;
	onClick: () => void;
}

const CopyButton = ({copied, disabled = false, label, onClick}: CopyButtonProps) => (
	<button type="button" className={styles.copyButton} onClick={onClick} disabled={disabled} aria-label={label}>
		{copied ? (
			<CheckCircleIcon className={styles.copyIcon} size={16} weight="bold" />
		) : (
			<ClipboardIcon className={styles.copyIcon} size={16} />
		)}
	</button>
);

CopyButton.displayName = 'CopyButton';

interface InitiateFormInputs {
	identifier: string;
}

interface VerifyFormInputs {
	_verify: string;
}

interface AddConnectionModalProps {
	defaultType?: ConnectionType;
}

export const AddConnectionModal = observer(({defaultType}: AddConnectionModalProps) => {
	const {t, i18n} = useLingui();
	const [step, setStep] = useState<'initiate' | 'verify'>('initiate');
	const [type, setType] = useState<ConnectionType>(defaultType ?? ConnectionTypes.BLUESKY);
	const [verificationData, setVerificationData] = useState<ConnectionVerificationResponse | null>(null);
	const [hostCopied, setHostCopied] = useState(false);
	const [valueCopied, setValueCopied] = useState(false);
	const pendingBlueskyHandle = useRef<string | null>(null);
	const initiateForm = useForm<InitiateFormInputs>();
	const verifyForm = useForm<VerifyFormInputs>();

	const connectionTypeOptions = useMemo(
		() => [
			{value: ConnectionTypes.BLUESKY, label: t`Bluesky`},
			{value: ConnectionTypes.DOMAIN, label: t`Domain`},
		],
		[t],
	);

	const handleTypeChange = useCallback((value: ConnectionType) => setType(value), []);

	const onSubmitInitiate = useCallback(
		async (data: InitiateFormInputs) => {
			let identifier = data.identifier.trim();
			if (type === ConnectionTypes.BLUESKY) {
				identifier = identifier.replace(/^https?:\/\/bsky\.app\/profile\//i, '').replace(/^@/, '');
			}
			if (UserConnectionStore.hasConnectionByTypeAndName(type, identifier)) {
				initiateForm.setError('identifier', {type: 'validate', message: t`You already have this connection.`});
				return;
			}
			if (type === ConnectionTypes.BLUESKY) {
				await ConnectionActionCreators.authorizeBlueskyConnection(i18n, identifier);
				pendingBlueskyHandle.current = identifier.toLowerCase();
				return;
			}
			const result = await ConnectionActionCreators.initiateConnection(i18n, type, identifier);
			setVerificationData(result);
			setStep('verify');
		},
		[i18n, initiateForm, t, type],
	);

	const onSubmitVerify = useCallback(async () => {
		if (!verificationData) return;
		await ConnectionActionCreators.verifyAndCreateConnection(i18n, verificationData.initiation_token);
		ModalActionCreators.pop();
	}, [i18n, verificationData]);

	const {handleSubmit: handleInitiateSubmit} = useFormSubmit({
		form: initiateForm,
		onSubmit: onSubmitInitiate,
		defaultErrorField: 'identifier',
	});

	const {handleSubmit: handleVerifySubmit} = useFormSubmit({
		form: verifyForm,
		onSubmit: onSubmitVerify,
		defaultErrorField: '_verify',
	});

	const hasBlueskyConnection = UserConnectionStore.hasConnectionByTypeAndName(
		ConnectionTypes.BLUESKY,
		pendingBlueskyHandle.current ?? '',
	);
	useEffect(() => {
		if (pendingBlueskyHandle.current && hasBlueskyConnection) {
			ModalActionCreators.pop();
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
		const success = await TextCopyActionCreators.copy(i18n, hostRecord);
		if (success) {
			notifyAndReset(setHostCopied);
		}
	}, [hostRecord, i18n, notifyAndReset]);

	const handleCopyValue = useCallback(async () => {
		if (!dnsValue) return;
		const success = await TextCopyActionCreators.copy(i18n, dnsValue);
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

	const hostCopyLabel = hostCopied ? t`Copied!` : t`Copy host`;
	const valueCopyLabel = valueCopied ? t`Copied!` : t`Copy value`;

	if (step === 'initiate') {
		return (
			<Modal.Root size="small" centered>
				<Form form={initiateForm} onSubmit={handleInitiateSubmit} aria-label={t`Add connection form`}>
					<Modal.Header title={t`Add Connection`} />
					<Modal.Content contentClassName={styles.content}>
						<div className={styles.stack}>
							<Select
								label={t`Connection Type`}
								value={type}
								options={connectionTypeOptions}
								onChange={handleTypeChange}
							/>
							<Input
								{...initiateForm.register('identifier', {required: true})}
								autoFocus={true}
								error={initiateForm.formState.errors.identifier?.message}
								label={type === ConnectionTypes.BLUESKY ? t`Handle` : t`Domain`}
								placeholder={type === ConnectionTypes.BLUESKY ? 'username.bsky.social' : 'example.com'}
								required={true}
							/>
						</div>
					</Modal.Content>
					<Modal.Footer>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button type="submit" submitting={initiateForm.formState.isSubmitting}>
							{type === ConnectionTypes.BLUESKY ? <Trans>Connect with Bluesky</Trans> : <Trans>Continue</Trans>}
						</Button>
					</Modal.Footer>
				</Form>
			</Modal.Root>
		);
	}

	return (
		<Modal.Root size="small" centered>
			<Form form={verifyForm} onSubmit={handleVerifySubmit} aria-label={t`Verify connection form`}>
				<Modal.Header title={t`Verify Connection`} />
				<Modal.Content contentClassName={styles.content}>
					<div className={styles.stack}>
						<p className={styles.instructions}>
							<Trans>Use the record below to prove domain ownership.</Trans>
						</p>
						<FormErrorText message={verifyForm.formState.errors._verify?.message} />
						<div className={styles.dnsCard}>
							<div className={styles.dnsHeading}>
								<p className={styles.dnsTitle}>
									<Trans>DNS TXT record</Trans>
								</p>
							</div>
							<div className={styles.dnsFields}>
								<Input
									label={t`Host`}
									value={hostRecord}
									readOnly={true}
									className={styles.dnsInput}
									rightElement={
										<CopyButton
											onClick={handleCopyHost}
											copied={hostCopied}
											disabled={!hostRecord}
											label={hostCopyLabel}
										/>
									}
								/>
								<Input
									label={t`Value`}
									value={dnsValue}
									readOnly={true}
									className={styles.dnsInput}
									rightElement={
										<CopyButton
											onClick={handleCopyValue}
											copied={valueCopied}
											disabled={!dnsValue}
											label={valueCopyLabel}
										/>
									}
								/>
							</div>
						</div>
						{dnsUrl && (
							<div className={styles.tokenCard}>
								<div className={styles.tokenCardHeader}>
									<p className={styles.tokenTitle}>
										<Trans>Serve the token file</Trans>
									</p>
									<p className={styles.tokenSubtitle}>
										<Trans>
											Download <code className={styles.inlineCode}>fluxer-verification</code> and place it in your{' '}
											<code className={styles.inlineCode}>.well-known</code> folder so we can validate the domain.
										</Trans>
									</p>
								</div>
								<div className={styles.tokenDownloadRow}>
									<Button
										type="button"
										variant="secondary"
										compact
										onClick={downloadTokenFile}
										disabled={!verificationData?.token}
									>
										<Trans>Download fluxer-verification</Trans>
									</Button>
								</div>
								<p className={styles.tokenMeta}>
									<Trans>
										The file contains the verification token we will fetch from{' '}
										<code className={styles.inlineCode}>{dnsUrl}</code>.
									</Trans>
								</p>
							</div>
						)}
					</div>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={() => setStep('initiate')} variant="secondary">
						<Trans>Back</Trans>
					</Button>
					<Button type="submit" submitting={verifyForm.formState.isSubmitting}>
						<Trans>Verify</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
