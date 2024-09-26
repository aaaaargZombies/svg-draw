import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

app.ports.logPort.subscribe((event) => {
  console.log(event);
});

app.ports.printPort.subscribe(async (_msg) => {
  // annoying hacky thing because Vite loads the styles differently to get HMR
  let styles = "";
  if (process.env.NODE_ENV === "development") {
    styles = document.querySelector("style").innerHTML;
  } else {
    const href = document
      .querySelector('link[rel="stylesheet"]')
      .getAttribute("href");

    styles = await fetch(href)
      .then((res) => res.text())
      .catch((_error) => "");
  }

  const styleNode = document.createElement("style");
  styleNode.innerHTML = styles;

  // must clone node or it will be modifying the view that Elm owns and everything goes out of sync
  const svg = document.querySelector("#SVG").cloneNode(true);
  svg.prepend(styleNode);

  const serializer = new XMLSerializer();
  const serializedSvg = serializer.serializeToString(svg);

  const link = document.createElement("a");
  const url =
    "data:image/svg+xml;charset=utf-8," + encodeURIComponent(serializedSvg);

  link.href = url;
  link.download = "doodle.svg";
  document.body.append(link);
  link.click();
  link.remove();
});
