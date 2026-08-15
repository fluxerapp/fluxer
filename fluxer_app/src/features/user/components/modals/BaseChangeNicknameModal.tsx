// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useCursorAtEnd} from '@app/features/app/hooks/useCursorAtEnd';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {CHANGE_NICKNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/user/components/modals/BaseChangeNicknameModal.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';
import {useForm} from 'react-hook-form';

const CHANGE_NICKNAME_FORM_DESCRIPTOR = msg({
	message: 'Change nickname form',
	comment: 'Short label in the base change nickname modal. Keep it concise.',
});
const NICKNAME_MUST_NOT_EXCEED_32_CHARACTERS_DESCRIPTOR = msg({
	message: 'Nickname must not exceed 32 characters',
	comment: 'Label in the base change nickname modal.',
});
const NICKNAME_DESCRIPTOR = msg({
	message: 'Nickname',
	comment: 'Short label in the base change nickname modal. Keep it concise.',
});
const CLEAR_NICKNAME_DESCRIPTOR = msg({
	message: 'Clear nickname',
	comment: 'Button or menu action label in the base change nickname modal. Keep it concise.',
});

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
		const {i18n} = useLingui();
		const form = useForm<FormInputs>({
			defaultValues: {
				nick: currentNick,
			},
		});
		const nickRef = useCursorAtEnd<HTMLInputElement>();
		const onSubmit = useCallback(
			async (data: FormInputs) => {
				const nick = data.nick.trim() || null;
				await onSave(nick);
				ToastCommands.createToast({
					type: 'success',
					children: <Trans>Nickname updated</Trans>,
				});
				ModalCommands.pop();
			},
			[onSave],
		);
		const {handleSubmit, isSubmitting} = useFormSubmit({
			form,
			onSubmit,
			defaultErrorField: 'nick',
		});
		const nickValue = form.watch('nick');
		const {ref: rhfRef, ...nickField} = form.register('nick', {
			maxLength: {
				value: 32,
				message: i18n._(NICKNAME_MUST_NOT_EXCEED_32_CHARACTERS_DESCRIPTOR),
			},
		});
		const latestRhfRef = useRef(rhfRef);
		latestRhfRef.current = rhfRef;
		const setInputRef = useCallback(
			(el: HTMLInputElement | null) => {
				nickRef(el);
				latestRhfRef.current(el);
			},
			[nickRef],
		);
		return (
			<Modal.Root size="small" centered data-flx="user.base-change-nickname-modal.modal-root">
				<Form
					form={form}
					onSubmit={handleSubmit}
					aria-label={i18n._(CHANGE_NICKNAME_FORM_DESCRIPTOR)}
					data-flx="user.base-change-nickname-modal.form.submit"
				>
					<Modal.Header
						title={i18n._(CHANGE_NICKNAME_DESCRIPTOR)}
						data-flx="user.base-change-nickname-modal.modal-header"
					/>
					<Modal.Content data-flx="user.base-change-nickname-modal.modal-content">
						<Modal.ContentLayout data-flx="user.base-change-nickname-modal.modal-content-layout">
							<Input
								data-flx="user.base-change-nickname-modal.input.text"
								{...nickField}
								ref={setInputRef}
								autoFocus={true}
								type="text"
								label={i18n._(NICKNAME_DESCRIPTOR)}
								placeholder={displayName}
								maxLength={32}
								error={form.formState.errors.nick?.message}
								rightElement={
									nickValue ? (
										<FocusRing offset={-2} data-flx="user.base-change-nickname-modal.focus-ring">
											<button
												type="button"
												className={styles.clearButton}
												onClick={() => form.setValue('nick', '')}
												aria-label={i18n._(CLEAR_NICKNAME_DESCRIPTOR)}
												data-flx="user.base-change-nickname-modal.clear-button.set-value"
											>
												<XIcon size={remFromPx(16)} weight="bold" data-flx="user.base-change-nickname-modal.x-icon" />
											</button>
										</FocusRing>
									) : undefined
								}
							/>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer data-flx="user.base-change-nickname-modal.modal-footer">
						<Button type="submit" submitting={isSubmitting} data-flx="user.base-change-nickname-modal.button.submit">
							<Trans>Save</Trans>
						</Button>
					</Modal.Footer>
				</Form>
			</Modal.Root>
		);
	},
);
