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
import {XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import styles from '~/components/modals/BaseChangeNicknameModal.module.css';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {useFormSubmit} from '~/hooks/useFormSubmit';

interface FormInputs {
	nick: string;
}

interface BaseChangeNicknameModalProps {
	currentNick: string;
	displayName: string;
	onSave: (nick: string | null) => Promise<void>;
}

export const BaseChangeNicknameModal: React.FC<BaseChangeNicknameModalProps> = observer(
	({currentNick, displayName, onSave}) => {
		const {t} = useLingui();
		const form = useForm<FormInputs>({
			defaultValues: {
				nick: currentNick,
			},
		});

		const onSubmit = React.useCallback(
			async (data: FormInputs) => {
				const nick = data.nick.trim() || null;

				await onSave(nick);

				ToastActionCreators.createToast({
					type: 'success',
					children: <Trans>Nickname updated</Trans>,
				});

				ModalActionCreators.pop();
			},
			[onSave],
		);

		const {handleSubmit, isSubmitting} = useFormSubmit({
			form,
			onSubmit,
			defaultErrorField: 'nick',
		});

		const nickValue = form.watch('nick');

		return (
			<Modal.Root size="small" centered>
				<Form form={form} onSubmit={handleSubmit} aria-label={t`Change nickname form`}>
					<Modal.Header title={t`Change Nickname`} />
					<Modal.Content className={confirmStyles.content}>
						<Input
							{...form.register('nick', {
								maxLength: {
									value: 32,
									message: t`Nickname must not exceed 32 characters`,
								},
							})}
							autoFocus={true}
							type="text"
							label={t`Nickname`}
							placeholder={displayName}
							maxLength={32}
							error={form.formState.errors.nick?.message}
							rightElement={
								nickValue ? (
									<FocusRing offset={-2}>
										<button
											type="button"
											className={styles.clearButton}
											onClick={() => form.setValue('nick', '')}
											aria-label={t`Clear nickname`}
										>
											<XIcon size={16} weight="bold" />
										</button>
									</FocusRing>
								) : undefined
							}
						/>
					</Modal.Content>
					<Modal.Footer>
						<Button type="submit" submitting={isSubmitting}>
							<Trans>Save</Trans>
						</Button>
					</Modal.Footer>
				</Form>
			</Modal.Root>
		);
	},
);
