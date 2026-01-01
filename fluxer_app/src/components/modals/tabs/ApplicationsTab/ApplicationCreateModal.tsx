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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {Input} from '~/components/form/Input';
import * as Modal from '~/components/modals/Modal';
import styles from '~/components/modals/tabs/ApplicationsTab/ApplicationsTab.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import type {DeveloperApplication} from '~/records/DeveloperApplicationRecord';

interface ApplicationCreateModalProps {
	onCreated: (application: DeveloperApplication) => void;
}

interface CreateFormValues {
	name: string;
}

export const ApplicationCreateModal: React.FC<ApplicationCreateModalProps> = observer(({onCreated}) => {
	const {t} = useLingui();
	const {
		register,
		handleSubmit,
		formState: {errors},
		reset,
	} = useForm<CreateFormValues>({
		defaultValues: {
			name: '',
		},
	});

	const nameField = register('name', {required: true, maxLength: 100});
	const nameInputRef = React.useRef<HTMLInputElement | null>(null);
	const [creating, setCreating] = React.useState(false);
	const [createError, setCreateError] = React.useState<string | null>(null);

	const handleCancel = () => {
		reset();
		setCreateError(null);
		ModalActionCreators.pop();
	};

	const onSubmit = handleSubmit(async (data) => {
		setCreateError(null);
		setCreating(true);
		try {
			const response = await HttpClient.post<DeveloperApplication>(Endpoints.OAUTH_APPLICATIONS, {
				name: data.name.trim(),
				redirect_uris: [],
			});
			onCreated(response.body);
			reset();
			ModalActionCreators.pop();
		} catch (err) {
			console.error('[ApplicationCreateModal] Failed to create application:', err);
			setCreateError(t`Failed to create application. Please check your inputs and try again.`);
		} finally {
			setCreating(false);
		}
	});

	return (
		<Modal.Root size="small" centered initialFocusRef={nameInputRef}>
			<form onSubmit={onSubmit}>
				<Modal.Header title={t`Create Application`} />
				<Modal.Content className={styles.createForm}>
					<Input
						type="text"
						label={t`Application Name`}
						{...nameField}
						ref={(el) => {
							nameField.ref(el);
							nameInputRef.current = el;
						}}
						placeholder={t`My Application`}
						maxLength={100}
						required
						disabled={creating}
						autoFocus
					/>

					{errors.name && <div className={styles.error}>{t`Application name is required`}</div>}
					{createError && <div className={styles.error}>{createError}</div>}
				</Modal.Content>
				<Modal.Footer>
					<Button type="button" variant="secondary" onClick={handleCancel} disabled={creating}>
						{t`Cancel`}
					</Button>
					<Button type="submit" variant="primary" submitting={creating}>
						{t`Create`}
					</Button>
				</Modal.Footer>
			</form>
		</Modal.Root>
	);
});
