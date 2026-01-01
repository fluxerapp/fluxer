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
import type React from 'react';
import {useId} from 'react';
import FormField from '~/components/auth/FormField';
import {Button} from '~/components/uikit/Button/Button';

type FieldErrors = Record<string, string | undefined> | null | undefined;

export type AuthFormControllerLike = {
	handleSubmit: (event: React.FormEvent<HTMLFormElement>) => void;
	getValue: (name: string) => string;
	setValue: (name: string, value: string) => void;
	getError: (name: string) => string | null | undefined;
	isSubmitting?: boolean;
};

export type AuthEmailPasswordFormClasses = {
	form: string;
};

type Props = {
	form: AuthFormControllerLike;
	isLoading: boolean;
	fieldErrors?: FieldErrors;
	submitLabel: React.ReactNode;
	classes: AuthEmailPasswordFormClasses;
	extraFields?: React.ReactNode;
	links?: React.ReactNode;
	linksWrapperClassName?: string;
	disableSubmit?: boolean;
};

export default function AuthLoginEmailPasswordForm({
	form,
	isLoading,
	fieldErrors,
	submitLabel,
	classes,
	extraFields,
	links,
	linksWrapperClassName,
	disableSubmit,
}: Props) {
	const {t} = useLingui();
	const emailId = useId();
	const passwordId = useId();

	const isSubmitting = Boolean(form.isSubmitting);
	const submitDisabled = isLoading || isSubmitting || Boolean(disableSubmit);

	return (
		<form className={classes.form} onSubmit={form.handleSubmit}>
			<FormField
				id={emailId}
				name="email"
				type="email"
				autoComplete="email"
				required
				label={t`Email`}
				value={form.getValue('email')}
				onChange={(value) => form.setValue('email', value)}
				error={form.getError('email') || fieldErrors?.email}
			/>

			<FormField
				id={passwordId}
				name="password"
				type="password"
				autoComplete="current-password"
				required
				label={t`Password`}
				value={form.getValue('password')}
				onChange={(value) => form.setValue('password', value)}
				error={form.getError('password') || fieldErrors?.password}
			/>

			{extraFields}

			{links ? <div className={linksWrapperClassName}>{links}</div> : null}

			<Button type="submit" fitContainer disabled={submitDisabled}>
				{typeof submitLabel === 'string' ? <Trans>{submitLabel}</Trans> : submitLabel}
			</Button>
		</form>
	);
}
