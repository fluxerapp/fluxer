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

import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, CheckCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';

import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Input} from '~/components/form/Input';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {IS_DEV} from '~/lib/env';
import HttpClient from '~/lib/HttpClient';
import RuntimeConfigStore, {describeApiEndpoint, type InstanceDiscoveryResponse} from '~/stores/RuntimeConfigStore';
import {isDesktop, openExternalUrl} from '~/utils/NativeUtils';

import styles from './BrowserLoginHandoffModal.module.css';

interface LoginSuccessPayload {
	token: string;
	userId: string;
}

interface BrowserLoginHandoffModalProps {
	onSuccess: (payload: LoginSuccessPayload) => Promise<void>;
	targetWebAppUrl?: string;
	prefillEmail?: string;
}

interface ValidatedInstance {
	apiEndpoint: string;
	webAppUrl: string;
}

type ModalView = 'main' | 'instance';

const CODE_LENGTH = 8;
const VALID_CODE_PATTERN = /^[A-Za-z0-9]{8}$/;

const normalizeEndpoint = (input: string): string => {
	const trimmed = input.trim();
	if (!trimmed) {
		throw new Error('API endpoint is required');
	}

	let candidate = trimmed;
	if (!/^[a-zA-Z][a-zA-Z0-9+\-.]*:\/\//.test(candidate)) {
		candidate = `https://${candidate}`;
	}

	const url = new URL(candidate);
	if (url.pathname === '' || url.pathname === '/') {
		url.pathname = '/api';
	}
	url.pathname = url.pathname.replace(/\/+$/, '');
	return url.toString();
};

const formatCodeForDisplay = (raw: string): string => {
	const cleaned = raw
		.replace(/[^A-Za-z0-9]/g, '')
		.toUpperCase()
		.slice(0, CODE_LENGTH);

	if (cleaned.length <= 4) {
		return cleaned;
	}
	return `${cleaned.slice(0, 4)}-${cleaned.slice(4)}`;
};

const extractRawCode = (formatted: string): string => {
	return formatted
		.replace(/[^A-Za-z0-9]/g, '')
		.toUpperCase()
		.slice(0, CODE_LENGTH);
};

const BrowserLoginHandoffModal = observer(
	({onSuccess, targetWebAppUrl, prefillEmail}: BrowserLoginHandoffModalProps) => {
		const {i18n} = useLingui();

		const [view, setView] = React.useState<ModalView>('main');
		const [code, setCode] = React.useState('');
		const [isSubmitting, setIsSubmitting] = React.useState(false);
		const [error, setError] = React.useState<string | null>(null);
		const inputRef = React.useRef<HTMLInputElement | null>(null);

		const [customInstance, setCustomInstance] = React.useState('');
		const [instanceValidating, setInstanceValidating] = React.useState(false);
		const [instanceError, setInstanceError] = React.useState<string | null>(null);
		const [validatedInstance, setValidatedInstance] = React.useState<ValidatedInstance | null>(null);

		const showInstanceOption = IS_DEV || isDesktop();

		const handleSubmit = React.useCallback(
			async (rawCode: string) => {
				if (!VALID_CODE_PATTERN.test(rawCode)) {
					return;
				}

				setIsSubmitting(true);
				setError(null);

				try {
					const customApiEndpoint = validatedInstance?.apiEndpoint;
					const result = await AuthenticationActionCreators.pollDesktopHandoffStatus(rawCode, customApiEndpoint);

					if (result.status === 'completed' && result.token && result.user_id) {
						if (customApiEndpoint) {
							await RuntimeConfigStore.connectToEndpoint(customApiEndpoint);
						}
						await onSuccess({token: result.token, userId: result.user_id});
						ModalActionCreators.pop();
						return;
					}

					if (result.status === 'pending') {
						setError(i18n._(msg`This code hasn't been used yet. Please complete login in your browser first.`));
					} else {
						setError(i18n._(msg`Invalid or expired code. Please try again.`));
					}
				} catch (err) {
					const message = err instanceof Error ? err.message : String(err);
					setError(message);
				} finally {
					setIsSubmitting(false);
				}
			},
			[i18n, onSuccess, validatedInstance],
		);

		const handleCodeChange = React.useCallback(
			(e: React.ChangeEvent<HTMLInputElement>) => {
				const rawCode = extractRawCode(e.target.value);
				setCode(rawCode);
				setError(null);

				if (VALID_CODE_PATTERN.test(rawCode)) {
					void handleSubmit(rawCode);
				}
			},
			[handleSubmit],
		);

		const handleOpenBrowser = React.useCallback(async () => {
			const currentWebAppUrl = RuntimeConfigStore.webAppBaseUrl;
			const baseUrl = validatedInstance?.webAppUrl || targetWebAppUrl || currentWebAppUrl;

			const params = new URLSearchParams({desktop_handoff: '1'});
			if (prefillEmail) {
				params.set('email', prefillEmail);
			}

			const url = `${baseUrl}/login?${params.toString()}`;
			await openExternalUrl(url);
		}, [prefillEmail, targetWebAppUrl, validatedInstance]);

		const handleShowInstanceView = React.useCallback(() => {
			setView('instance');
		}, []);

		const handleBackToMain = React.useCallback(() => {
			setView('main');
			setInstanceError(null);
		}, []);

		const handleSaveInstance = React.useCallback(async () => {
			if (!customInstance.trim()) {
				setInstanceError(i18n._(msg`Please enter an API endpoint.`));
				return;
			}

			setInstanceValidating(true);
			setInstanceError(null);

			try {
				const apiEndpoint = normalizeEndpoint(customInstance);
				const instanceUrl = `${apiEndpoint}/instance`;

				const response = await HttpClient.get<InstanceDiscoveryResponse>({url: instanceUrl});

				if (!response.ok) {
					const status = String(response.status);
					throw new Error(i18n._(msg`Failed to reach instance (${status})`));
				}

				const instance = response.body;
				if (!instance.endpoints?.webapp) {
					throw new Error(i18n._(msg`Invalid instance response: missing webapp URL.`));
				}

				const webAppUrl = instance.endpoints.webapp.replace(/\/$/, '');
				setValidatedInstance({apiEndpoint, webAppUrl});
				setView('main');
			} catch (err) {
				const message = err instanceof Error ? err.message : String(err);
				setInstanceError(message);
			} finally {
				setInstanceValidating(false);
			}
		}, [customInstance, i18n]);

		const handleClearInstance = React.useCallback(() => {
			setValidatedInstance(null);
			setCustomInstance('');
			setInstanceError(null);
		}, []);

		React.useEffect(() => {
			if (view === 'main') {
				inputRef.current?.focus();
			}
		}, [view]);

		if (view === 'instance') {
			return (
				<Modal.Root size="small" centered onClose={ModalActionCreators.pop}>
					<Modal.Header title={i18n._(msg`Custom instance`)} />
					<Modal.Content className={styles.content}>
						<Input
							label={i18n._(msg`API Endpoint`)}
							type="url"
							placeholder="https://api.example.com"
							value={customInstance}
							onChange={(e) => {
								setCustomInstance(e.target.value);
								setInstanceError(null);
							}}
							error={instanceError ?? undefined}
							disabled={instanceValidating}
							footer={
								!instanceError ? (
									<p className={styles.inputHelper}>
										<Trans>Enter the API endpoint of the Fluxer instance you want to connect to.</Trans>
									</p>
								) : null
							}
							autoFocus
						/>
					</Modal.Content>
					<Modal.Footer>
						<Button variant="secondary" onClick={handleBackToMain} disabled={instanceValidating}>
							<Trans>Back</Trans>
						</Button>
						<Button
							variant="primary"
							onClick={handleSaveInstance}
							disabled={instanceValidating || !customInstance.trim()}
						>
							{instanceValidating ? <Trans>Checking...</Trans> : <Trans>Save</Trans>}
						</Button>
					</Modal.Footer>
				</Modal.Root>
			);
		}

		return (
			<Modal.Root size="small" centered onClose={ModalActionCreators.pop}>
				<Modal.Header title={i18n._(msg`Add account`)} />
				<Modal.Content className={styles.content}>
					<p className={styles.description}>
						<Trans>Log in using your browser, then enter the code shown to add the account.</Trans>
					</p>

					<div className={styles.codeInputSection}>
						<Input
							ref={inputRef}
							label={i18n._(msg`Login code`)}
							value={formatCodeForDisplay(code)}
							onChange={handleCodeChange}
							error={error ?? undefined}
							disabled={isSubmitting}
							autoComplete="off"
						/>
					</div>

					{validatedInstance ? (
						<div className={styles.instanceBadge}>
							<CheckCircleIcon size={14} weight="fill" className={styles.instanceBadgeIcon} />
							<span className={styles.instanceBadgeText}>
								<Trans>Using {describeApiEndpoint(validatedInstance.apiEndpoint)}</Trans>
							</span>
							<button type="button" className={styles.instanceBadgeClear} onClick={handleClearInstance}>
								<Trans>Clear</Trans>
							</button>
						</div>
					) : showInstanceOption ? (
						<button type="button" className={styles.instanceLink} onClick={handleShowInstanceView}>
							<Trans>I want to use a custom Fluxer instance</Trans>
						</button>
					) : null}

					{prefillEmail ? (
						<p className={styles.prefillHint}>
							<Trans>We will prefill {prefillEmail} once the browser login opens.</Trans>
						</p>
					) : null}
				</Modal.Content>

				<Modal.Footer>
					<Button variant="secondary" onClick={ModalActionCreators.pop} disabled={isSubmitting}>
						<Trans>Cancel</Trans>
					</Button>
					<Button variant="primary" onClick={handleOpenBrowser} submitting={isSubmitting}>
						<ArrowSquareOutIcon size={16} weight="bold" />
						<Trans>Open browser</Trans>
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);

export function showBrowserLoginHandoffModal(
	onSuccess: (payload: LoginSuccessPayload) => Promise<void>,
	targetWebAppUrl?: string,
	prefillEmail?: string,
): void {
	ModalActionCreators.push(
		modal(() => (
			<BrowserLoginHandoffModal
				onSuccess={async (payload) => {
					await onSuccess(payload);
				}}
				targetWebAppUrl={targetWebAppUrl}
				prefillEmail={prefillEmail}
			/>
		)),
	);
}

export default BrowserLoginHandoffModal;
