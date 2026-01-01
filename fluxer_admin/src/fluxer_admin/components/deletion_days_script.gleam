//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import lustre/attribute as a
import lustre/element/html as h

pub fn render() {
  let script =
    "(function(){const configs=[{selectId:'user-deletion-reason',inputId:'user-deletion-days'},{selectId:'bulk-deletion-reason',inputId:'bulk-deletion-days'}];const userReason='1';const userMin=14;const defaultMin=60;const update=(select,input)=>{const min=select.value===userReason?userMin:defaultMin;input.min=min.toString();const current=parseInt(input.value,10);if(isNaN(current)||current<min){input.value=min.toString();}};configs.forEach(({selectId,inputId})=>{const select=document.getElementById(selectId);const input=document.getElementById(inputId);if(!select||!input){return;}select.addEventListener('change',()=>update(select,input));update(select,input);});})();"

  h.script([a.attribute("defer", "defer")], script)
}
