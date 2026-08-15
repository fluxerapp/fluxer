// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {observer} from 'mobx-react-lite';
import qrCode from 'qrcode';
import {useEffect, useRef} from 'react';

const logger = new Logger('QRCodeCanvas');
export const QRCodeCanvas = observer(({data, size = 100}: {data: string; size?: number}) => {
	const canvasRef = useRef<HTMLCanvasElement>(null);
	useEffect(() => {
		const canvas = canvasRef.current;
		const qrSize = size;
		const padding = 10;
		const totalSize = qrSize + padding * 2;
		if (canvas) {
			canvas.width = totalSize;
			canvas.height = totalSize;
			const context = canvas.getContext('2d');
			if (context) {
				context.fillStyle = 'white';
				context.fillRect(0, 0, totalSize, totalSize);
				context.fillStyle = 'white';
				context.beginPath();
				context.moveTo(padding, 0);
				context.lineTo(totalSize - padding, 0);
				context.quadraticCurveTo(totalSize, 0, totalSize, padding);
				context.lineTo(totalSize, totalSize - padding);
				context.quadraticCurveTo(totalSize, totalSize, totalSize - padding, totalSize);
				context.lineTo(padding, totalSize);
				context.quadraticCurveTo(0, totalSize, 0, totalSize - padding);
				context.lineTo(0, padding);
				context.quadraticCurveTo(0, 0, padding, 0);
				context.closePath();
				context.fill();
				const tempCanvas = document.createElement('canvas');
				qrCode.toCanvas(
					tempCanvas,
					data,
					{width: qrSize, margin: 0, color: {dark: '#000000', light: '#FFFFFF00'}},
					(error: Error | null | undefined) => {
						if (error) {
							logger.error(error);
						} else {
							context.drawImage(tempCanvas, padding, padding);
						}
					},
				);
			}
		}
	}, [data, size]);
	return (
		<canvas
			ref={canvasRef}
			style={{borderRadius: remFromPx(10), backgroundColor: 'white'}}
			data-flx="ui.qr-code-canvas.canvas"
		/>
	);
});
