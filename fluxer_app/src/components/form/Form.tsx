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

import {observer} from 'mobx-react-lite';
import type {FieldValues, UseFormReturn} from 'react-hook-form';

type FormProps<T extends FieldValues> = Omit<React.HTMLAttributes<HTMLFormElement>, 'onSubmit'> & {
	form: UseFormReturn<T>;
	onSubmit: (values: T) => void;
	'aria-label'?: string;
	'aria-labelledby'?: string;
};

export const Form = observer(
	<T extends FieldValues>({
		form,
		onSubmit,
		children,
		'aria-label': ariaLabel,
		'aria-labelledby': ariaLabelledBy,
		...props
	}: FormProps<T>) => (
		<form
			{...props}
			aria-label={ariaLabel || undefined}
			aria-labelledby={ariaLabelledBy || undefined}
			style={{display: 'contents', ...props.style}}
			onSubmit={(event) => {
				event.preventDefault();
				form.clearErrors();
				form.handleSubmit(onSubmit)(event);
			}}
		>
			{children}
		</form>
	),
);
