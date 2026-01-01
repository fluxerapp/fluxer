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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {Form} from '~/components/form/Form';
import styles from '~/components/modals/AccountDeleteModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import * as RouterUtils from '~/utils/RouterUtils';

export const AccountDeleteModal = observer(() => {
	const {t} = useLingui();
	const form = useForm();

	const onSubmit = async () => {
		await UserActionCreators.deleteAccount();
		ModalActionCreators.pop();
		RouterUtils.transitionTo('/login');
	};

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'form',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Delete account form`}>
				<Modal.Header title={t`Delete Account`} />
				<Modal.Content className={styles.content}>
					<div className={styles.infoSection}>
						<p>
							<Trans>
								Are you sure you want to delete your account? This action will schedule your account for permanent
								deletion.
							</Trans>
						</p>
						<div className={styles.infoBox}>
							<p className={styles.infoBoxTitle}>
								<Trans>Important information:</Trans>
							</p>
							<ul className={styles.infoList}>
								<li>
									<Trans>You can cancel the deletion process within 14 days</Trans>
								</li>
								<li>
									<Trans>After 14 days, your account will be permanently deleted</Trans>
								</li>
								<li>
									<Trans>Once deletion is processed, you cannot recover access to your account</Trans>
								</li>
								<li>
									<Trans>You will not be able to delete your sent messages after your account is deleted</Trans>
								</li>
							</ul>
						</div>
						<p className={styles.disclaimer}>
							<Trans>
								If you want to export your data or delete your messages first, please visit the Privacy Dashboard
								section in User Settings before proceeding.
							</Trans>
						</p>
					</div>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting} variant="danger-primary">
						<Trans>Delete Account</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
