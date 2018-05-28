import { Controller } from "stimulus";

const DOUBLE_CLICK_THRESHOLD = 420; // blaze it

export default class extends Controller {
  static targets = ["slider"];

  lastSliderClick = Date.now();

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

  handleDoubleClick() {
    const diff = Date.now() - this.lastSliderClick;
    this.lastSliderClick = Date.now();

    if (diff < DOUBLE_CLICK_THRESHOLD) this.resetSlider();
  }

  resetSlider() {
    this.sliderTarget.value = 0;

    this.adjustBrightness({ target: { value: 0 } });
  }
}
