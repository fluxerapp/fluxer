// SPDX-License-Identifier: AGPL-3.0-or-later

import {Slider} from '@app/features/ui/components/Slider';
import styles from '@app/features/user/components/modals/tabs/appearance_tab/StepSliderRow.module.css';
import type React from 'react';
import {useId, useMemo} from 'react';

interface StepSliderRowProps {
	label: string;
	stops: ReadonlyArray<number>;
	value: number;
	defaultValue: number;
	labelledStops: ReadonlyArray<number>;
	formatValue: (value: number) => string;
	formatAccessibleValue: (value: number) => string;
	onChange: (value: number) => void;
	action?: React.ReactNode;
	dataFlx: string;
}

function stopIndex(stops: ReadonlyArray<number>, value: number): number {
	const index = stops.indexOf(value);
	return index === -1 ? 0 : index;
}

export function StepSliderRow({
	label,
	stops,
	value,
	defaultValue,
	labelledStops,
	formatValue,
	formatAccessibleValue,
	onChange,
	action,
	dataFlx,
}: StepSliderRowProps): React.ReactElement {
	const labelId = useId();
	const indices = useMemo(() => stops.map((_, index) => index), [stops]);
	const valueIndex = stopIndex(stops, value);
	return (
		<div className={styles.row} data-flx={`${dataFlx}.row`}>
			<div className={styles.header} data-flx={`${dataFlx}.header`}>
				<div className={styles.titleRow} data-flx={`${dataFlx}.title-row`}>
					<span id={labelId} className={styles.label} data-flx={`${dataFlx}.label`}>
						{label}
					</span>
					{action}
				</div>
				<span className={styles.value} aria-hidden="true" data-flx={`${dataFlx}.value`}>
					{formatValue(value)}
				</span>
			</div>
			<Slider
				defaultValue={valueIndex}
				value={valueIndex}
				factoryDefaultValue={stopIndex(stops, defaultValue)}
				minValue={0}
				maxValue={stops.length - 1}
				step={1}
				markers={indices}
				stickToMarkers={true}
				markerPosition="below"
				ariaLabelledBy={labelId}
				ariaValueText={formatAccessibleValue(value)}
				onMarkerRender={(index) => {
					const stop = stops[index];
					return labelledStops.includes(stop) ? formatValue(stop) : '';
				}}
				onValueChange={(index) => {
					const next = stops[Math.round(index)];
					if (next !== undefined && next !== value) onChange(next);
				}}
				data-flx={`${dataFlx}.slider`}
			/>
		</div>
	);
}
