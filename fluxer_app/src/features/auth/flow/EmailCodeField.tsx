// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import FormField from '@app/features/auth/flow/AuthFormField';
import {isCompleteEmailCode, normalizeEmailCode} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {VERIFICATION_CODE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useEffect, useId, useRef} from 'react';

const SIX_DIGIT_CODE_PLACEHOLDER_DESCRIPTOR = msg({
	message: '6-digit code',
	comment: 'Placeholder in the field where people type the code we emailed them.',
});

interface EmailCodeFieldProps {
	readonly value: string;
	readonly error?: string | null;
	readonly disabled?: boolean;
	readonly onChange: (value: string) => void;
	readonly onComplete?: (code: string) => void;
	readonly inputRef?: React.Ref<HTMLInputElement>;
}

export function EmailCodeField({value, error, disabled, onChange, onComplete, inputRef}: EmailCodeFieldProps) {
	const {i18n} = useLingui();
	const id = useId();
	const fieldRef = useRef<HTMLInputElement | null>(null);
	const mergedRef = useMergeRefs([fieldRef, inputRef]);
	const wasDisabledRef = useRef(disabled === true);
	useEffect(() => {
		const wasDisabled = wasDisabledRef.current;
		wasDisabledRef.current = disabled === true;
		const focusLost = document.activeElement === null || document.activeElement === document.body;
		if (wasDisabled && !disabled && (error || focusLost)) {
			fieldRef.current?.focus();
			fieldRef.current?.select();
		}
	}, [disabled, error]);
	const handleChange = (raw: string) => {
		const next = normalizeEmailCode(raw);
		onChange(next);
		if (isCompleteEmailCode(next) && next !== value) {
			onComplete?.(next);
		}
	};
	return (
		<FormField
			inputRef={mergedRef}
			id={id}
			name="code"
			type="text"
			inputMode="numeric"
			autoComplete="one-time-code"
			autoCapitalize="none"
			autoCorrect="off"
			enterKeyHint="done"
			spellCheck={false}
			autoFocus
			data-step-focus="true"
			required
			disabled={disabled}
			label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
			placeholder={i18n._(SIX_DIGIT_CODE_PLACEHOLDER_DESCRIPTOR)}
			value={value}
			onChange={handleChange}
			error={error ?? undefined}
			data-flx="auth.flow.email-code-field.form-field"
		/>
	);
}
