import { Controller } from "stimulus";

export default class extends Controller {
  adjustBrightness({ target: { value } }) {
    const distance = parseFloat(value, 10);

    const checkVisibility = theme => {
      const { brightness } = theme.dataset;
      const index = parseFloat(brightness, 10);

      const isVisible = distance < 0 ? index < 1 + distance : index > distance;
      theme.style.display = isVisible ? "initial" : "none";
    };

    // a more descriptive name would be nice
    const collection = document.getElementById("wrapper");

    collection.childNodes.forEach(checkVisibility);
  }
}
