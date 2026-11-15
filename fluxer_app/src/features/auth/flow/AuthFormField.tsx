// SPDX-License-Identifier: AGPL-3.0-or-later

import {Input} from '@app/features/ui/components/form/FormInput';
import {observer} from 'mobx-react-lite';

interface FormFieldProps extends Omit<React.InputHTMLAttributes<HTMLInputElement>, 'value' | 'onChange'> {
	name: string;
	label?: React.ReactNode;
	value: string;
	error?: string;
	placeholder?: string;
	onChange: (value: string) => void;
	inputRef?: React.Ref<HTMLInputElement>;
}

const FormField = observer(function FormField({
	name,
	label,
	value,
	error,
	onChange,
	inputRef,
	...props
}: FormFieldProps) {
	return (
		<Input
			ref={inputRef}
			name={name}
			label={typeof label === 'string' ? label : undefined}
			value={value}
			error={error}
			onChange={(e) => onChange(e.target.value)}
			data-flx="auth.flow.form-field.input.change"
			{...props}
		/>
	);
});

export default FormField;
